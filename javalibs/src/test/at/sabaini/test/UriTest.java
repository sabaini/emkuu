package at.sabaini.test;

import at.sabaini.emkuuclient.Uri;

import org.junit.Before;
import org.junit.Test;
import static org.junit.Assert.*;

public class UriTest {

	private Uri u;
	private Uri u2;

	@Before
	public void setUp() throws Exception {
		this.u = Uri.decode("emkuu://emkuu/foo/bar?gugu=hubu");
		this.u2 = Uri.decode("emkuu://emkuu/a");
	}

	@Test
	public void testGetPath() {
		assertArrayEquals(new String[]{"foo", "bar"}, this.u.getPath().toArray());
	}

	@Test
	public void testGetQry() {
		assertEquals("gugu=hubu", this.u.getQry());
	}
	
	@Test
	public void testToString() {
		String s = this.u.toString();
		assertEquals("emkuu://emkuu/foo/bar?gugu=hubu", s);
	}
	
	@Test 
	public void testToS2() {
		String s = this.u2.toString();
		assertEquals("emkuu://emkuu/a", s);
	}
	
	@Test
	public void testPathString() {
		String s = this.u.pathString();
		assertEquals("/foo/bar", s);
	}
}
