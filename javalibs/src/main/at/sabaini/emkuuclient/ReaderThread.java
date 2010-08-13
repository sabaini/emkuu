package at.sabaini.emkuuclient;

import java.io.IOException;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;

import at.sabaini.emkuuclient.msg.AbstractMessage;
import at.sabaini.emkuuclient.msg.OkMsg;

import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.XMLReaderFactory;

public class ReaderThread extends Thread implements IMsgHandler, IInitHandler {

	private XMLReader xmlReader;
	private InputSource is;
	private BlockingQueue<AbstractMessage> recvq = new ArrayBlockingQueue<AbstractMessage>(20);
	public BlockingQueue<String> okq = new ArrayBlockingQueue<String>(5);
	public BlockingQueue<MsgId> initq = new ArrayBlockingQueue<MsgId>(1);

	public ReaderThread(InputSource is) {
		super();
		this.is = is;
		try {
			this.xmlReader = XMLReaderFactory.createXMLReader();
			this.xmlReader.setContentHandler(new EmkuuContentHandler(this, this));
			this.start();
		} catch (SAXException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	public void run() {
		try {
			this.xmlReader.parse(this.is);
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (SAXException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}	
	}

	public AbstractMessage get() throws InterruptedException {
		return this.recvq.take();
	}

	@Override
	public void handle(AbstractMessage m) {
		try {
			if( m instanceof OkMsg ) {
				this.okq.put(m.msgid);
			} else {
				this.recvq.put(m);
			}
		} catch (InterruptedException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();			
		}

	}

	@Override
	public void handleInit(MsgId msgid) {
		try {
			this.initq.put(msgid);
		} catch (InterruptedException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

}
