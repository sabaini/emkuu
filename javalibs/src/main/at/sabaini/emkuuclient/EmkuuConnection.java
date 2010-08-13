package at.sabaini.emkuuclient;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PrintStream;
import java.net.Socket;
import java.net.UnknownHostException;
import java.util.HashMap;
import java.util.Map;

import at.sabaini.emkuuclient.msg.AbstractMessage;
import at.sabaini.emkuuclient.msg.StateMsg;

import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

public class EmkuuConnection {

	private ReaderThread reader;
	private WriterThread writer;
	private Map<String, Boolean> okMap = new HashMap<String, Boolean>();
	private Socket skt;
	private PrintStream os;
	private InputSource is;
	private WorkerThread listeners;
	private MsgId curmsgid;
		
	private synchronized void initskt(String host, int port) {
		try {
			this.skt = new Socket(host, port);
			this.is = new InputSource(this.skt.getInputStream());
			this.os = new PrintStream(this.skt.getOutputStream(), true);
		} catch (UnknownHostException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	
	private synchronized void finishskt() {
		this.os.print(EmkuuContentHandler.STREAMTAIL);
		this.os.close();
		try {
			this.skt.close();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	
	
	public void connect(String host, int port) throws UnknownHostException, IOException, SAXException {
		this.initskt(host, port);
		this.init();
	}
	
	public void connect(OutputStream os, InputStream is) throws IOException, SAXException {
		this.os = new PrintStream(os);
		this.is = new InputSource(is);
		this.init();
	}
	
	private void init() {
		this.reader = new ReaderThread(this.is);
		this.writer = new WriterThread(this.os);
		this.listeners = new WorkerThread(this.reader, this.writer);
		this.os.println(EmkuuContentHandler.STREAMHEAD);
		try {
			this.curmsgid = this.reader.initq.take();
		} catch (InterruptedException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	public synchronized void send(AbstractMessage msg) {
		msg.msgid = this.curmsgid.toString();
		this.writer.put(msg);
		this.curmsgid.num += 1;
		if(this.okMap.get(msg.msgid) != null) {
			this.okMap.remove(msg.msgid);
		} else {
			while(true) {
				try {
					String ackid = this.reader.okq.take();
					if(ackid.equals(msg.msgid)) break;
					this.okMap.put(ackid, true);
				} catch (InterruptedException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			}
		}
	}
	
	public synchronized void send(AbstractMessage msg, IEmkuuMsgCallback cb) {
		this.listeners.setCallback(msg.corrid, cb);
		this.send(msg);
	}
	
	public synchronized void addListener(IEmkuuMsgListener l) {
		this.listeners.addListener(l);
	}
	
	public void close() {
		this.reader.interrupt();
		this.writer.interrupt();
		this.listeners.interrupt();
		this.finishskt();
	}
}
