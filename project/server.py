import asyncio
import argparse
import sys
import time
import aiohttp
import json

server_names = {
"Riley": 15850, 
"Jaquez": 15851, 
"Juzang": 15852, 
"Campbell": 15853, 
"Bernard": 15854
}

server_connections = {
    'Riley': ['Jaquez', 'Juzang'],
    'Bernard': ['Jaquez', 'Juzang', 'Campbell'],
    'Juzang': ['Campbell', 'Bernard', 'Riley'],
    'Campbell': ['Juzang', 'Bernard'],
    'Jaquez': ['Riley', 'Bernard']
}

clients = dict()

KEY = 'AIzaSyDHi3VGHYg3Ks0eGTd7U-5pW9DYPkG4u8g'
class Server:
    def __init__(self, name, port, ip='127.0.0.1', message_max_length=1e6):
        self.name = name
        self.ip = ip
        self.port = port
        self.message_max_length = int(message_max_length)

    async def handle_echo(self, reader, writer):
        """
        on server side
        """
        data = await reader.read(self.message_max_length)
        message = data.decode()
        addr = writer.get_extra_info('peername')
        log_file.write("{} received {} from {}.\n".format(self.name, message, addr))
        #split the message
        substrings = message.split()        
        #IAMAT case
        if substrings[0] == "IAMAT":
            if len(substrings) != 4:
                sendback_message = "? " + message
            elif substrings[2].rfind('+') == -1 or substrings[2].rfind('-') == -1:
                sendback_message = "? " + message
            else:
                diff = str(time.time() - float(substrings[3]))
                if diff[0] != '-':
                    diff = '+' + diff
                sendback_message = "AT {} {} {} {} {}".format(self.name, diff, substrings[1], substrings[2], substrings[3])
                clients[substrings[1]] = [substrings[2], substrings[3], diff, self.name]
                await self.flood(sendback_message)
        #WHATSAT case
        elif substrings[0] == "WHATSAT":
            if len(substrings) != 4:
                sendback_message = "? " + message
            elif int(substrings[2]) < 0 or int(substrings[2]) > 50:
                sendback_message = "? " + message
            elif int(substrings[3]) < 0 or int(substrings[3]) > 20:
                sendback_message = "? " + message
            else:
                client_ID = substrings[1]
                if client_ID in clients:
                    coordinate, time_client_sent, time_diff, server_name = clients[client_ID]
                    sendback_message = "AT {} {} {} {} {}".format(server_name, time_diff, client_ID, coordinate, time_client_sent)
                    radius = str(int(substrings[2]) * 1000)
                    upper_bound = substrings[3]
                    plus_loc = coordinate.rfind('+')
                    minus_loc = coordinate.rfind('-')
                    if  plus_loc == 0:
                        latitude = coordinate[0:minus_loc]
                        longitude =coordinate[minus_loc:]
                    else:
                        latitude = coordinate[plus_loc:]
                        longitude =coordinate[0:plus_loc]
                    url = 'https://maps.googleapis.com/maps/api/place/nearbysearch/json?location={},{}&radius={}&key={}'.format(latitude,longitude, radius, KEY)
                    async with aiohttp.ClientSession() as session:
                        async with session.get(url) as resp:
                            result = await resp.text()
                    result_json = json.loads(result)
                    if len(result_json["results"]) > int(upper_bound):
                        result_json['results'] = result_json['results'][:int(upper_bound)]
                    formatted_result_json = json.dumps(result_json, indent=4)
                    sendback_message += formatted_result_json + "\n"
                else:
                    sendback_message = "? " + message
        #AT case
        elif substrings[0] == "AT":
            sendback_message = ""
            if len(substrings) != 6:
                sendback_message = "? " + message
            else:
                if substrings[3] in clients:
                    if float(substrings[5]) >float( clients[substrings[3]][1]):
                        clients[substrings[3]] = [substrings[4], substrings[5], substrings[2], substrings[1]]
                        #log_file.write("The existing client has been updated\n")
                        await self.flood(message)
                else:
                    clients[substrings[3]] = [substrings[4], substrings[5], substrings[2], substrings[1]]
                    #log_file.write("The client has been added\n")
                    await self.flood(message)
        else:
            sendback_message = "? " + message
        if substrings[0] != "AT":
            writer.write(sendback_message.encode())
            log_file.write("{} sent: {}\n".format(self.name, sendback_message))
            await writer.drain()
            log_file.write("close the client socket.\n")
            writer.close()

    async def flood(self, message):
        for serv in server_connections[self.name]:
            try:
                reader,writer = await asyncio.open_connection(host='127.0.0.1', port=server_names[serv])
                log_file.write("Succeed to connect the server: " + serv + "\n")
                writer.write(message.encode())
                await writer.drain()
                log_file.write("flood message sent: " + message + "\n")
                writer.write_eof()
                writer.close()
            except:
                log_file.write("Failed to connect the server: " + serv)
                pass

    async def run_forever(self):
        server = await asyncio.start_server(self.handle_echo, self.ip, self.port)

        # Serve requests until Ctrl+C is pressed
        print(f'serving on {server.sockets[0].getsockname()}')
        async with server:
            await server.serve_forever()
        # Close the server
        server.close()


def main():
    if(len(sys.argv) != 2):
        print('Wrong number of arguments: needs appropriate server name and port')
        exit(1)
    parser = argparse.ArgumentParser('CS131 project argument parser')
    parser.add_argument('server_name', type=str,
                        help='required server name input')
    args = parser.parse_args()
    if not args.server_name in server_names:
        print("Invalid Server Name.")
        exit(1)
    print("Hello, welcome to server {}".format(args.server_name))
    file_name = args.server_name + "_log.txt"
    global log_file
    log_file = open(file_name, 'w+')
    #clean the file
    log_file.truncate(0)
    server = Server(args.server_name, server_names[args.server_name])
    try:
        asyncio.run(server.run_forever())
    except KeyboardInterrupt:
        pass
    log_file.close()

if __name__ == '__main__':
    main()
