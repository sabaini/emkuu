package at.sabaini.emkuuclient;

import java.util.ArrayList;
import java.util.List;

import at.sabaini.emkuuclient.msg.AbstractMessage;
import at.sabaini.emkuuclient.msg.OkMsg;

public class WorkerThread extends Thread {
	private ReaderThread source;
	private WriterThread writer;
	private List<IEmkuuMsgListener> listeners = new ArrayList<IEmkuuMsgListener>();
	
	public WorkerThread(ReaderThread reader, WriterThread writer) {
		this.source = reader;
		this.writer = writer;
		this.listeners.add(new CbHandler());
		this.start();
	}
	
	public synchronized void setCallback(Long corrid, IEmkuuMsgCallback cb) {
		//System.out.println("setcb: " + corrid);
		((CbHandler)this.listeners.get(0)).cbMap.put(corrid, cb);
	}
	
	public void addListener(IEmkuuMsgListener l) {
		this.listeners.add(l);
	}
	
	public void sndok(String msgid) {
		OkMsg o = new OkMsg();
		o.msgid = msgid;
		this.writer.put(o);
	}
	
	public void run() {
		while(!this.isInterrupted()) {
			try {
				AbstractMessage msg = this.source.get();
				String msgid = msg.msgid;
				System.out.println("worker: got " + msgid);
				for(IEmkuuMsgListener l : this.listeners) {
					System.out.println(l);
					msg = l.handle(msg);
					if(msg == null) break;
				}
				if( msg != null ){
					System.out.println("WARN: msg fell of the end, msgid: " + msgid + ", corrid: " + msg.corrid);
				}
				this.sndok(msgid);
			} catch (InterruptedException e) { }
			
		}
	}
}
