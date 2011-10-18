package com.aicontest.visualizer;

import java.util.AbstractQueue;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.NoSuchElementException;
import java.util.PriorityQueue;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.ReentrantLock;

import com.aicontest.visualizer.js.tasks.DelayedExecutionUnit;
import com.aicontest.visualizer.js.tasks.EventExecutionUnit;
import com.aicontest.visualizer.js.tasks.IExecutionUnit;

public class ExecutionUnitQueue extends AbstractQueue<IExecutionUnit> implements
		BlockingQueue<IExecutionUnit> {

	private transient final ReentrantLock lock = new ReentrantLock();
	private transient final Condition available = lock.newCondition();
	private final LinkedList<EventExecutionUnit> immediate = new LinkedList<EventExecutionUnit>();
	private final PriorityQueue<DelayedExecutionUnit> delayed = new PriorityQueue<DelayedExecutionUnit>();

	public void put(IExecutionUnit eu) {
		offer(eu);
	}

	public boolean offer(IExecutionUnit eu, long timeout, TimeUnit unit) {
		return offer(eu);
	}

	public boolean offer(IExecutionUnit eu) {
		final ReentrantLock lock = this.lock;
		lock.lock();
		try {
			if (eu instanceof DelayedExecutionUnit) {
				DelayedExecutionUnit deu = (DelayedExecutionUnit) eu;
				DelayedExecutionUnit first = delayed.peek();
				delayed.offer(deu);
				if (first == null || deu.compareTo(first) < 0)
					available.signalAll();
			} else {
				immediate.offer((EventExecutionUnit) eu);
				available.signalAll();
			}
			return true;
		} finally {
			lock.unlock();
		}
	}

	public IExecutionUnit poll() {
		final ReentrantLock lock = this.lock;
		lock.lock();
		try {
			IExecutionUnit eu = immediate.peek();
			if (eu == null) {
				DelayedExecutionUnit deu = delayed.peek();
				if (deu == null || deu.getDelay(TimeUnit.NANOSECONDS) > 0) {
					return null;
				} else {
					IExecutionUnit x = delayed.poll();
					assert x != null;
					if (delayed.size() != 0)
						available.signalAll();
					return x;
				}
			} else {
				IExecutionUnit x = immediate.poll();
				assert x != null;
				if (immediate.size() != 0)
					available.signalAll();
				return x;
			}
		} finally {
			lock.unlock();
		}
	}

	public IExecutionUnit take() throws InterruptedException {
		final ReentrantLock lock = this.lock;
		lock.lockInterruptibly();
		try {
			for (;;) {
				IExecutionUnit eu = immediate.peek();
				if (eu != null) {
					IExecutionUnit x = immediate.poll();
					assert x != null;
					if (immediate.size() != 0)
						available.signalAll();
					return x;
				}
				DelayedExecutionUnit deu = delayed.peek();
				if (deu != null) {
					long delay = deu.getDelay(TimeUnit.NANOSECONDS);
					if (delay > 0) {
						available.awaitNanos(delay);
					} else {
						IExecutionUnit x = delayed.poll();
						assert x != null;
						if (delayed.size() != 0)
							available.signalAll(); // wake up other takers
						return x;
					}
				} else {
					available.await();
				}
			}
		} finally {
			lock.unlock();
		}
	}

	public IExecutionUnit poll(long timeout, TimeUnit unit)
			throws InterruptedException {
		long nanos = unit.toNanos(timeout);
		final ReentrantLock lock = this.lock;
		lock.lockInterruptibly();
		try {
			for (;;) {
				IExecutionUnit eu = immediate.peek();
				if (eu != null) {
					IExecutionUnit x = immediate.poll();
					assert x != null;
					if (immediate.size() != 0)
						available.signalAll();
					return x;
				}
				DelayedExecutionUnit deu = delayed.peek();
				if (deu == null) {
					if (nanos <= 0)
						return null;
					else
						nanos = available.awaitNanos(nanos);
				} else {
					long delay = deu.getDelay(TimeUnit.NANOSECONDS);
					if (delay > 0) {
						if (nanos <= 0)
							return null;
						if (delay > nanos)
							delay = nanos;
						long timeLeft = available.awaitNanos(delay);
						nanos -= delay - timeLeft;
					} else {
						IExecutionUnit x = delayed.poll();
						assert x != null;
						if (delayed.size() != 0)
							available.signalAll();
						return x;
					}
				}
			}
		} finally {
			lock.unlock();
		}
	}

	public IExecutionUnit peek() {
		final ReentrantLock lock = this.lock;
		lock.lock();
		try {
			IExecutionUnit eu = immediate.peek();
			if (eu != null) {
				return eu;
			}
			return delayed.peek();
		} finally {
			lock.unlock();
		}
	}

	public int size() {
		final ReentrantLock lock = this.lock;
		lock.lock();
		try {
			return immediate.size() + delayed.size();
		} finally {
			lock.unlock();
		}
	}

	public int drainTo(Collection<? super IExecutionUnit> c) {
		if (c == null)
			throw new NullPointerException();
		if (c == this)
			throw new IllegalArgumentException();
		final ReentrantLock lock = this.lock;
		lock.lock();
		try {
			c.addAll(immediate);
			int n = immediate.size();
			immediate.clear();
			int i = n;
			for (;;) {
				DelayedExecutionUnit first = delayed.peek();
				if (first == null || first.getDelay(TimeUnit.NANOSECONDS) > 0)
					break;
				c.add(delayed.poll());
				++n;
			}
			if (n > i)
				available.signalAll();
			return n;
		} finally {
			lock.unlock();
		}
	}

	public int drainTo(Collection<? super IExecutionUnit> c, int maxElements) {
		if (c == null)
			throw new NullPointerException();
		if (c == this)
			throw new IllegalArgumentException();
		if (maxElements <= 0)
			return 0;
		final ReentrantLock lock = this.lock;
		lock.lock();
		try {
			int n;
			if (immediate.size() > maxElements) {
				n = 0;
				while (n < maxElements) {
					c.add(immediate.poll());
					++n;
				}
			} else {
				c.addAll(immediate);
				n = immediate.size();
				immediate.clear();
				int i = n;
				while (n < maxElements) {
					DelayedExecutionUnit first = delayed.peek();
					if (first == null
							|| first.getDelay(TimeUnit.NANOSECONDS) > 0)
						break;
					c.add(delayed.poll());
					++n;
				}
				if (n > i)
					available.signalAll();
			}
			return n;
		} finally {
			lock.unlock();
		}
	}

	public void clear() {
		final ReentrantLock lock = this.lock;
		lock.lock();
		try {
			immediate.clear();
			delayed.clear();
		} finally {
			lock.unlock();
		}
	}

	public int remainingCapacity() {
		return Integer.MAX_VALUE;
	}

	public Object[] toArray() {
		final ReentrantLock lock = this.lock;
		lock.lock();
		try {
			ArrayList<IExecutionUnit> al = new ArrayList<IExecutionUnit>(
					immediate.size() + delayed.size());
			al.addAll(immediate);
			al.addAll(delayed);
			return al.toArray();
		} finally {
			lock.unlock();
		}
	}

	public <T> T[] toArray(T[] a) {
		final ReentrantLock lock = this.lock;
		lock.lock();
		try {
			ArrayList<IExecutionUnit> al = new ArrayList<IExecutionUnit>(
					immediate.size() + delayed.size());
			al.addAll(immediate);
			al.addAll(delayed);
			return al.toArray(a);
		} finally {
			lock.unlock();
		}
	}

	public boolean remove(Object o) {
		final ReentrantLock lock = this.lock;
		lock.lock();
		try {
			return immediate.remove(o) || delayed.remove(o);
		} finally {
			lock.unlock();
		}
	}

	public Iterator<IExecutionUnit> iterator() {
		return new Itr(toArray());
	}

	private class Itr implements Iterator<IExecutionUnit> {
		final Object[] array;
		int cursor;
		int lastRet;

		Itr(Object[] array) {
			lastRet = -1;
			this.array = array;
		}

		public boolean hasNext() {
			return cursor < array.length;
		}

		public IExecutionUnit next() {
			if (cursor >= array.length)
				throw new NoSuchElementException();
			lastRet = cursor;
			return (IExecutionUnit) array[cursor++];
		}

		public void remove() {
			if (lastRet < 0)
				throw new IllegalStateException();
			Object x = array[lastRet];
			lastRet = -1;
			lock.lock();
			try {
				for (Iterator<?> it = immediate.iterator(); it.hasNext();) {
					if (it.next() == x) {
						it.remove();
						return;
					}
				}
				for (Iterator<?> it = delayed.iterator(); it.hasNext();) {
					if (it.next() == x) {
						it.remove();
						return;
					}
				}
			} finally {
				lock.unlock();
			}
		}
	}

	public void compressEvent(EventExecutionUnit task) {
		final ReentrantLock lock = this.lock;
		lock.lock();
		try {
			Iterator<EventExecutionUnit> it = immediate.iterator();
			while (it.hasNext()) {
				EventExecutionUnit eeu = it.next();
				if (eeu.matches(task))
					it.remove();
			}
			immediate.offer(task);
			available.signalAll();
		} finally {
			lock.unlock();
		}
	}
}