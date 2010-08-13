package at.sabaini.emkuuclient.msg;

import java.io.StringWriter;
import java.lang.reflect.Field;
import java.util.HashMap;

import javax.xml.stream.XMLOutputFactory;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;

import at.sabaini.emkuuclient.Uri;

public abstract class AbstractMessage  {	
	public String msgid; // xxx should be a MsgId
	public Uri fro;
	public Uri to;
	public Long corrid;
	public String date;
	private HashMap<String, String> classMap = new HashMap<String, String>();

	public AbstractMessage() {
		classMap.put("at.sabaini.emkuuclient.msg.GetMsg", "get");
		classMap.put("at.sabaini.emkuuclient.msg.PutMsg", "put");
		classMap.put("at.sabaini.emkuuclient.msg.DeleteMsg", "delete");
		classMap.put("at.sabaini.emkuuclient.msg.PostMsg", "post");
		classMap.put("at.sabaini.emkuuclient.msg.RouteMsg", "route");
		classMap.put("at.sabaini.emkuuclient.msg.StateMsg", "state");
		classMap.put("at.sabaini.emkuuclient.msg.SubscribeMsg", "subscribe");	
		classMap.put("at.sabaini.emkuuclient.msg.OkMsg", "ok");
	}

	public String getShortName() {
		return "e:" + this.classMap.get(this.getClass().getName());
	}
	public String toString() {
		return "<" + this.getShortName() + " msgid=\"" + this.msgid + "\" />";
	}
	public String toXml() {
		XMLOutputFactory factory = XMLOutputFactory.newInstance();
		StringWriter buf = new StringWriter();
		try {
			XMLStreamWriter writer = factory.createXMLStreamWriter(buf);
			writer.writeStartElement(this.getShortName());
			this.writeAttrsAndBody(writer);
			writer.writeEndElement();
			writer.flush();
			writer.close();
		} catch (XMLStreamException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IllegalArgumentException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		buf.flush();
		return buf.toString();
	}

	private void writeAttrsAndBody(XMLStreamWriter writer) {
		this.writeAttrs(writer);
	}

	protected void writeAttrs(XMLStreamWriter writer) {
		Field[] flds = this.getClass().getFields();
		try {
			for(int i=0; i<flds.length; i++) {
				Field f = flds[i];
				Object o = f.get(this);
				if(o == null) continue;
				if((o instanceof Boolean) && o.equals(false)) continue;
				writer.writeAttribute(f.getName(),	o.toString());
			}
		} catch (IllegalArgumentException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (XMLStreamException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IllegalAccessException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
}
