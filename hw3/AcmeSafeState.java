import java.util.concurrent.atomic.AtomicLongArray;
class AcmeSafeState implements State {
    private AtomicLongArray value;

    AcmeSafeState(int length) { value = new AtomicLongArray(length); }

    public int size() { return value.length(); }

    public long[] current() {
	long[] converted_value = new long[size()];
	for(int i = 0; i < size(); i++){
	    converted_value[i] = value.get(i);
	}
	return converted_value;
    }

    public void swap(int i, int j) {
	value.getAndDecrement(i);
        value.getAndIncrement(j);
    }
}
