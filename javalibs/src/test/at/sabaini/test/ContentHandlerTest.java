package at.sabaini.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.io.StringReader;
import java.util.Stack;

import at.sabaini.emkuuclient.EmkuuContentHandler;
import at.sabaini.emkuuclient.msg.AbstractMessage;
import at.sabaini.emkuuclient.msg.GetMsg;
import at.sabaini.emkuuclient.msg.PutMsg;
import at.sabaini.emkuuclient.msg.RouteMsg;

import org.junit.Before;
import org.junit.Test;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.XMLReaderFactory;

public class ContentHandlerTest {

	static final String STREAMHEAD = "<stream:stream xmlns:stream=\"http://etherx.jabber.org/streams\" " 
		+ "xmlns:e=\"http://sabaini.at/protocol/emkuu-0.1\" version=\"1.0\" current_msgid=\"test:1\">";
	private XMLReader reader;
	private EmkuuContentHandler emkuuct;
	private String putxmlstr = "<e:put fro=\"emkuu://sender@somewhere\"  to=\"emkuu://rcpt@somewhere\" msgid=\"23\" corrid=\"12\" date=\"2008-12-01T23:59\">A Message Body w./ Newlines &amp; Entities </e:put></stream:stream>";
	private String putnestedxmlstr = "<e:put fro=\"emkuu://sender@somewhere\"  to=\"emkuu://rcpt@somewhere\" msgid=\"23\" corrid=\"12\" date=\"2008-12-01T23:59\"><stream:doc stream:a=\"123\">abc</stream:doc></e:put></stream:stream>";
	private String routexmlstr = "<e:route to=\"emkuu://emkuu/foo\" fro=\"emkuu://emkuu/test\"><e:body>abc</e:body><e:enclosed><![CDATA[<doc>123</doc>]]></e:enclosed></e:route></stream:stream>";
	private String nestedroutexmlstr = "<e:route to=\"emkuu://emkuu/foo\" fro=\"emkuu://emkuu/test\"><e:body>abc</e:body><e:enclosed><e:get to=\"emkuu://emkuu/a\" /></e:enclosed></e:route></stream:stream>";
	private String nestedroutebodyxmlstr = "<e:route to=\"emkuu://emkuu/foo\" fro=\"emkuu://emkuu/test\"><e:body><doc /></e:body><e:enclosed><e:get to=\"emkuu://emkuu/a\" /></e:enclosed></e:route></stream:stream>";
	private String nestedroutebodyxmlstr2 = "<e:route to=\"emkuu://emkuu/foo\" fro=\"emkuu://emkuu/test\"><e:body><doc>123</doc></e:body><e:enclosed /></e:route></stream:stream>";
	private String nestedroutensbodyxmlstr = "<e:route to=\"emkuu://emkuu/foo\" fro=\"emkuu://emkuu/test\"><e:body><stream:doc>123</stream:doc></e:body><e:enclosed /></e:route></stream:stream>";
	private String simplexml = "</stream:stream>";
	private InputSource putinput;
	private InputSource routeinput;
	private InputSource nestedinput;
	private InputSource simpleinput;
	private TestMsgHandler testhdlr;
	private InputSource nestedbodyinput;
	private InputSource nestedbodyinput2;
	private InputSource nestedbodynsinput;
	private InputSource putnestedinput;

		
	@Before
	public void setUp() throws Exception {
		this.testhdlr = new TestMsgHandler();
		emkuuct = new EmkuuContentHandler(this.testhdlr, this.testhdlr);
		try {
			reader = XMLReaderFactory.createXMLReader(  );
			reader.setContentHandler(emkuuct);
		} catch (SAXException e) {
			e.printStackTrace();
		}
		putinput = new InputSource(new StringReader(ContentHandlerTest.STREAMHEAD + this.putxmlstr));
		putnestedinput = new InputSource(new StringReader(ContentHandlerTest.STREAMHEAD + this.putnestedxmlstr));
		routeinput = new InputSource(new StringReader(ContentHandlerTest.STREAMHEAD + this.routexmlstr));
		nestedinput = new InputSource(new StringReader(ContentHandlerTest.STREAMHEAD + this.nestedroutexmlstr));
		nestedbodyinput = new InputSource(new StringReader(ContentHandlerTest.STREAMHEAD + this.nestedroutebodyxmlstr));
		nestedbodyinput2 = new InputSource(new StringReader(ContentHandlerTest.STREAMHEAD + this.nestedroutebodyxmlstr2));
		nestedbodynsinput = new InputSource(new StringReader(ContentHandlerTest.STREAMHEAD + this.nestedroutensbodyxmlstr));		
		simpleinput = new InputSource(new StringReader(ContentHandlerTest.STREAMHEAD + this.simplexml));		
	}

	@Test
	public void testParse() throws IOException, SAXException {
		this.reader.parse(this.simpleinput);
	}

	@Test
	public void testStartElement() throws IOException, SAXException {
		this.reader.parse(this.putinput);
		Stack<AbstractMessage> s = this.emkuuct.getStack();
		assertEquals(0, s.size());
		AbstractMessage m = this.testhdlr.msg;
		assertEquals("at.sabaini.emkuuclient.msg.PutMsg", m.getClass().getName());
		assertEquals(12, m.corrid);
	}

	@Test
	public void testNestedPut() throws IOException, SAXException {
		this.reader.parse(this.putnestedinput);
		Stack<AbstractMessage> s = this.emkuuct.getStack();
		assertEquals(0, s.size());
		AbstractMessage m = this.testhdlr.msg;
		assertEquals("at.sabaini.emkuuclient.msg.PutMsg", m.getClass().getName());
		assertEquals(12, m.corrid);
		PutMsg p = (PutMsg)m;
		assertEquals("<stream:doc stream:a=\"123\">abc</stream:doc>", p.body);
	}	
	
	@Test
	public void testEndElement() throws IOException, SAXException {
		this.reader.parse(this.putinput);
		PutMsg m = (PutMsg)this.testhdlr.msg;
		assertEquals("A Message Body w./ Newlines & Entities ", m.body);
	}

	@Test
	public void testRouteMsg() throws IOException, SAXException {
		this.reader.parse(this.routeinput);
		RouteMsg m = (RouteMsg)this.testhdlr.msg;
		assertEquals("abc", m.body);
		assertEquals("<doc>123</doc>", m.enclosed);
	}
	
	@Test
	public void testNestedRoute() throws IOException, SAXException {
		this.reader.parse(this.nestedinput);
		RouteMsg m = (RouteMsg)this.testhdlr.msg;
		Object g = m.enclosed;
		assertTrue(g instanceof GetMsg);
	}

	@Test
	public void testNestedBodyRoute() throws IOException, SAXException {
		this.reader.parse(this.nestedbodyinput);
		RouteMsg m = (RouteMsg)this.testhdlr.msg;
		Object g = m.enclosed;
		assertTrue(g instanceof GetMsg);
		assertEquals("<doc />", m.body);
	}

	@Test
	public void testNestedBodyRoute2() throws IOException, SAXException {
		this.reader.parse(this.nestedbodyinput2);
		RouteMsg m = (RouteMsg)this.testhdlr.msg;
		assertEquals("<doc>123</doc>", m.body);
	}

	@Test
	public void testNestedBodyNSRoute() throws IOException, SAXException {
		this.reader.parse(this.nestedbodynsinput);
		RouteMsg m = (RouteMsg)this.testhdlr.msg;
		assertEquals("<stream:doc>123</stream:doc>", m.body);
	}

}
