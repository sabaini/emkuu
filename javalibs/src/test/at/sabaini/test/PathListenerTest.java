package at.sabaini.test;


import org.junit.Before;
import org.junit.Test;

import static org.junit.Assert.assertEquals;

import at.sabaini.emkuuclient.PathListener;
import at.sabaini.emkuuclient.Uri;
import at.sabaini.emkuuclient.msg.AbstractMessage;
import at.sabaini.emkuuclient.msg.RouteMsg;

public class PathListenerTest {

	class TestListener extends PathListener {

		public AbstractMessage msg;
		public String rest;

		public TestListener(String pathre) { super(pathre); }

		@Override
		public AbstractMessage handleMatch(AbstractMessage msg, String rest) {
			this.msg = msg;
			this.rest = rest;
			return null;
		}
		
	}
	
	@Before
	public void setUp() throws Exception {
	}

	@Test
	public void testMatch() {
		TestListener l = new TestListener("/emkuu/something/");
		RouteMsg msg = new RouteMsg();
		msg.to = Uri.decode("emkuu://emkuu/something/a");
		Object r = l.handle(msg);
		assertEquals(l.msg, msg);
		assertEquals(r, null);
	}
	@Test
	public void testRest() {
		TestListener l = new TestListener("/emkuu/something/");
		RouteMsg msg = new RouteMsg();
		msg.to = Uri.decode("emkuu://emkuu/something/a/b/c");
		Object r = l.handle(msg);
		assertEquals(l.msg, msg);
		assertEquals("a/b/c", l.rest);
		assertEquals(r, null);
	}
	@Test
	public void testNonMatch() {
		TestListener l = new TestListener("/emkuu/something/.*");
		RouteMsg msg = new RouteMsg();
		msg.to = Uri.decode("emkuu://emkuu/other");
		Object r = l.handle(msg);		
		assertEquals(r, msg);
		assertEquals(l.msg, null);
	}

}
