package at.sabaini.emkuuclient;

import java.io.IOException;
import java.net.UnknownHostException;

import org.xml.sax.SAXException;

public class EmkuuManagedConnection extends Thread {

	private EmkuuConnection connection;

	public EmkuuManagedConnection(int port, 
			IEmkuuListenerFactory listenerFactory, 
			Object[] listenerArgs ) {
		this("localhost", port, listenerFactory, listenerArgs);
	}
	
	public EmkuuManagedConnection(String host, int port, 
			IEmkuuListenerFactory listenerFactory, 
			Object[] listenerArgs ) {
		super();
		this.connection = new EmkuuConnection();
		try {
			IEmkuuMsgListener l = listenerFactory.create(this.connection, this, listenerArgs);
			this.connection.connect(host, port);
			this.connection.addListener(l);
		} catch (UnknownHostException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (SAXException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	
	public synchronized void run() {
		while(!this.isInterrupted())
			try { this.wait(); } catch (InterruptedException e) {}
	}
}
