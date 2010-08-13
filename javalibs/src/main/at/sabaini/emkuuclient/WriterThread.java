package at.sabaini.emkuuclient;

import java.io.PrintStream;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;

import at.sabaini.emkuuclient.msg.AbstractMessage;

public class WriterThread extends Thread {
	private BlockingQueue<AbstractMessage> sendq = new ArrayBlockingQueue<AbstractMessage>(20);
	private PrintStream os;

	public WriterThread(PrintStream printStream) {
		super();
		this.os = printStream;
		this.start();
	}

	public void put(AbstractMessage msg) {
		try {
			this.sendq.put(msg);
		} catch (InterruptedException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	
	public void run() {
		AbstractMessage m; 
		while( ! this.isInterrupted()) {
			try {
				m = this.sendq.take();
				//System.out.println("\nsending: " + m.toXml() + "\n");
				this.os.print(m.toXml());
			} catch (InterruptedException e) {}
			
		}
	}
	
}
