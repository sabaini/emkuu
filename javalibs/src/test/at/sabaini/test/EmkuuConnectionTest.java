package at.sabaini.test;

import static org.junit.Assert.fail;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.UnsupportedEncodingException;
import java.net.ServerSocket;

import at.sabaini.emkuuclient.EmkuuConnection;
import at.sabaini.emkuuclient.EmkuuContentHandler;
import at.sabaini.emkuuclient.msg.GetMsg;

import org.junit.Before;
import org.junit.Test;
import org.xml.sax.SAXException;

public class EmkuuConnectionTest {
	private EmkuuConnection con;
	private ServerSocket skt;
	private OutputStream os; // stuff comes from connection
	private InputStream is;  // stuff goes to connection
	private String streamopen = "<e:ok msgid=\"test:1\" /></stream:stream>";
	private String getxmlstr = 
		"<e:get fro=\"emkuu://emkuu/test\"  to=\"emkuu://emkuu/test\" msgid=\"23\" corrid=\"12\" date=\"2008-12-01T23:59\" /></stream:stream>";
	private String routexmlstr = "<e:route to=\"emkuu://emkuu/foo\" fro=\"emkuu://emkuu/test\"><e:body>123</e:body><e:enclosed><![CDATA[<doc>abc</doc>]]></e:enclosed></e:route></stream:stream>";
	private String nestedroutexmlstr = "<e:route to=\"emkuu://emkuu/foo\" fro=\"emkuu://emkuu/test\"><e:body>123</e:body><e:enclosed><e:get to=\"emkuu://emkuu/a\" /></e:enclosed></e:route></stream:stream>";
	private TestListener listener;
	private String ackstr = "<e:ok msgid=\"test:1\" /><e:ok msgid=\"test:2\" />";

	@Before
	public void setUp() throws Exception {
		this.os = new ByteArrayOutputStream();
		this.con = new EmkuuConnection();
		this.listener = new TestListener();
	}

	private void conStr(String s) {
		String pre = ContentHandlerTest.STREAMHEAD;
		System.out.println(pre+s);
		try {
			this.is = new ByteArrayInputStream((pre + s).getBytes("UTF-8"));
			this.con.connect(os, is);
			try { Thread.sleep(200); } catch (InterruptedException e) {}
		} catch (UnsupportedEncodingException e) {
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

	private void conStrWithListener(String s) {
		String pre = ContentHandlerTest.STREAMHEAD; 
		try {
			this.is = new ByteArrayInputStream((pre + s).getBytes("UTF-8"));
			this.con.connect(os, is);
			this.con.addListener(this.listener);
			try { Thread.sleep(600); } catch (InterruptedException e) {}
		} catch (UnsupportedEncodingException e) {
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

	@Test
	public void testSend() {
		this.conStr(this.streamopen);
		this.con.send(new GetMsg());
		try { Thread.sleep(300); } catch (InterruptedException e) {}		
		String s = this.os.toString();
		assertTrue(s.length() > 0);
	}

	@Test
	public void testListener() {
		this.conStrWithListener(this.getxmlstr);
		try { Thread.sleep(700); } catch (InterruptedException e) {}
		assertTrue(this.listener.msg.corrid == 12);
	}

	@Test
	public void testCallback() {
		TestCallback cb = new TestCallback();
		GetMsg m = new GetMsg();
		m.corrid = 12L;
		m.msgid = "test:1";

		String s = this.ackstr
		+ m.toXml()	
		+ EmkuuContentHandler.STREAMTAIL;		
		try {
			this.is = new ByteArrayInputStream((ContentHandlerTest.STREAMHEAD + s).getBytes("UTF-8"));
			this.con.connect(os, is);
			this.con.send(m, cb);
			try { Thread.sleep(200); } catch (InterruptedException e) {}
		} catch (UnsupportedEncodingException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (SAXException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		try { Thread.sleep(400); } catch (InterruptedException e) {}
		assertTrue(cb.msg.corrid == 12);
	}

}
