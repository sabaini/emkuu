package at.sabaini.test;

import static org.junit.Assert.assertEquals;
import at.sabaini.emkuuclient.Uri;
import at.sabaini.emkuuclient.msg.GetMsg;
import at.sabaini.emkuuclient.msg.PostMsg;
import at.sabaini.emkuuclient.msg.RouteMsg;
import at.sabaini.emkuuclient.msg.StateMsg;

import org.junit.Before;
import org.junit.Test;

public class MsgTest {

	
	private GetMsg g;
	private StateMsg s;
	private RouteMsg r;

	@Before
	public void setUp() throws Exception {
		this.g = new GetMsg();
		this.g.fro = Uri.decode("emkuu://emkuu/a");
		this.g.msgid = "23";
		this.g.corrid = 42L;
		this.s = new StateMsg();
		this.s.contentlen = 1L;
		this.s.body = "test";
		this.r = new RouteMsg();
		this.r.enclosed = "e";
		this.r.body = "b";
	}

	@Test
	public void testGetToXml() {
		String s = this.g.toXml();
		assertEquals("<e:get msgid=\"23\" fro=\"emkuu://emkuu/a\" corrid=\"42\"></e:get>", s);
	}
	@Test 
	public void testStateToXml() {
		String s = this.s.toXml();
		assertEquals("<e:state status=\"200\" contentlen=\"1\">test</e:state>", s);
	}
	
	@Test
	public void testRouteToXml() {
		String s = this.r.toXml();
		assertEquals("<e:route><body>b</body><enclosed>e</enclosed></e:route>", s);
	}
	
	@Test
	public void testPostToXml() {
		PostMsg p = new PostMsg();
		p.corrid = 23L;
		p.body = "foo";
		assertEquals("<e:post corrid=\"23\">foo</e:post>", 
				p.toXml());
	}
	
	@Test
	public void testNestedRoute() {
		RouteMsg r2 = new RouteMsg();
		this.r.enclosed = r2;
		r2.enclosed = this.g;
		String s = this.r.toXml();
		assertEquals("<e:route><body>b</body><enclosed><e:route><body></body><enclosed><e:get msgid=\"23\" fro=\"emkuu://emkuu/a\" corrid=\"42\"></e:get></enclosed></e:route></enclosed></e:route>", 
				s);
		
	}

}
