package at.sabaini.emkuuclient;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.Stack;

import at.sabaini.emkuuclient.msg.AbstractMessage;
import at.sabaini.emkuuclient.msg.EntityMessage;
import at.sabaini.emkuuclient.msg.RouteMsg;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

public class EmkuuContentHandler extends DefaultHandler {
	public static String STREAMHEAD="<stream:stream xmlns:stream=\"http://etherx.jabber.org/streams\" xmlns:e=\"http://sabaini.at/protocol/emkuu-0.1\" version=\"1.0\">";
	public static String STREAMTAIL="</stream:stream>";
	public static String EMKUUNS="http://sabaini.at/protocol/emkuu-0.1";
	public static String STREAMNS="http://etherx.jabber.org/streams";

	private Stack<AbstractMessage> stack;
	private boolean docStarted;
	private HashMap<String, String> classMap = new HashMap<String, String>();
	private StringBuilder chars;
	private IMsgHandler hdlr;
	private boolean openElem;
	private IInitHandler inithdlr;
		
	public EmkuuContentHandler(IMsgHandler msghdlr, IInitHandler inithdlr) {
		super();
		this.hdlr = msghdlr;
		this.inithdlr = inithdlr;
		this.classMap.put("get", "GetMsg");
		this.classMap.put("put", "PutMsg");
		this.classMap.put("delete", "DeleteMsg");
		this.classMap.put("post", "PostMsg");
		this.classMap.put("route", "RouteMsg");
		this.classMap.put("state", "StateMsg");
		this.classMap.put("subscribe", "SubscribeMsg");
		this.classMap.put("ok", "OkMsg");
	}
	
	public Stack<AbstractMessage> getStack() {
		return stack;
	}

	public void setStack(Stack<AbstractMessage> stack) {
		this.stack = stack;
	}
	
	@Override
	public void characters(char[] ch, int start, int len) throws SAXException {
		// TODO Auto-generated method stub
        String text = new String( ch, start, len );
        if(this.stack.empty()) return;
        if(this.openElem) {
        	this.chars.append(">");
        	this.openElem = false;
        }
        this.chars.append(text);
	}

	@Override
	public void endDocument() throws SAXException {
		this.docStarted = false;
	}

	@Override
	public void endElement(String nsURI, String localName, String qName) throws SAXException {
		if(nsURI.equals(EmkuuContentHandler.EMKUUNS)) 
			this.endEmkuu(localName);
		else
			this.endOther(nsURI, qName);
	}

	private void endOther(String nsURI, String qName) {
		if(this.openElem) {
			this.chars.append(" />");
			this.openElem = false;
		} else {
			this.chars.append("</" + qName + ">");
		}
	}

	private void endEmkuu(String localName) throws SAXException {
		System.out.println("end " + localName);
		if(this.stack.empty()) return;
		this.openElem = false;
		if(localName.equals("body")) {
			RouteMsg r = (RouteMsg)this.stack.peek();
			r.body = this.chars.toString();
			this.chars = new StringBuilder();
			return;
		}
		AbstractMessage m = this.stack.pop();
		String text = this.chars.toString();
		this.chars = new StringBuilder();
		if (m instanceof EntityMessage) {
			if( m instanceof RouteMsg ) {
				RouteMsg r2 = (RouteMsg)m;
				r2.enclosed = text;
			} else {
				EntityMessage m2 = (EntityMessage) m;
				m2.body = text;
			}
		}
		AbstractMessage a;
		RouteMsg r;
		while(!this.stack.empty()) {
			a = this.stack.pop();
			if(!( a instanceof RouteMsg))
				throw new SAXException("Illegal nesting: " + a);
			r = (RouteMsg)a;
			r.enclosed = m;
			m = r;
		}
		this.hdlr.handle(m);
		System.out.println("crea/handle msg: " + m.msgid + ", corrid: " + m.corrid);
	}

	@Override
	public void startDocument() throws SAXException {
		this.docStarted = true;
		this.stack = new Stack<AbstractMessage>();
		this.chars = new StringBuilder();
		this.openElem = false;
	}
		
	@Override
	public void startElement(String nsURI, String localName,
			String rawName, Attributes attributes) throws SAXException {
		System.out.println("ste " + rawName);
		if(!this.docStarted) return; 
		if (localName.equals("stream") && nsURI.equals(EmkuuContentHandler.STREAMNS)) {
			this.doHandleInit(attributes);
			return;
		}
		if(nsURI.equals(EmkuuContentHandler.EMKUUNS)) 
			this.startEmkuu(localName, attributes);
		else
			this.startOther(nsURI, rawName, attributes);		
	}
	
	private void doHandleInit(Attributes attributes) {
		String s = attributes.getValue("current_msgid");
		this.inithdlr.handleInit(new MsgId(s));
	}

	private void startOther(String nsURI, String rawName, Attributes attributes) {
		this.chars.append("<" + rawName);
		this.addAttrs(attributes);
		this.openElem = true;
	}
	
	private void addAttrs(Attributes attributes) {
		String n, v;
		for(int i=0; i<attributes.getLength(); i++){
			n = attributes.getQName(i);
			v = attributes.getValue(i);
			this.chars.append(" " + n + "=\"" + v + "\"");
		}
	}

	private void startEmkuu(String localName, Attributes attributes) throws SAXException {
		if(this.stack.size() > 0 && this.stack.peek() instanceof RouteMsg) {
			if(localName.equals("body") || localName.equals("enclosed")) {
				//this.chars = new StringBuilder();
				return;
			}
		}
		String klassname = this.classMap.get(localName);
		if(klassname != null) {
			this.handleMsgTag(klassname, attributes);
		} else {
			throw new SAXException("Unknown emkuu tag: " + localName);
		}	
	}
	
	private void handleMsgTag(String klassname, Attributes attributes) {
		Class klass;
		AbstractMessage m;
		try {
			klass = Class.forName("at.sabaini.emkuuclient.msg." + klassname);
			m = (AbstractMessage)klass.newInstance();
			Field[] flds = klass.getFields();
			Field f;
			for(int i=0; i<flds.length; i++) {
				f = flds[i];
				String n = f.getName();
				String v = attributes.getValue(n);
				if(v != null) {
					Class k = f.getType();
					if(k == String.class ) {
						f.set(m, v);
					} else {
						//System.out.println("dec: " + klassname + ", field: " + f);
						Method decoder = k.getMethod("decode", new Class[] { String.class });
						f.set(m, decoder.invoke(null, new Object[] {v})); 
					}
				}
			}
			this.stack.push(m);
		} catch (InstantiationException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		} catch (IllegalAccessException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		} catch (ClassNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IllegalArgumentException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (SecurityException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (NoSuchMethodException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (InvocationTargetException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}		
	}



}
