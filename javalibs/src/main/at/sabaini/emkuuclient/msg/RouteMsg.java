package at.sabaini.emkuuclient.msg;

import java.lang.reflect.Field;

import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;

public class RouteMsg extends EntityMessage {
	public String authorization;
	public String replyto;
	public Object enclosed; // String or AbstractMessage
	
	protected void writeAttrs(XMLStreamWriter writer) {
		Field[] flds = this.getClass().getFields();
		try {
			String b = "";
			String e = "";
			for(int i=0; i<flds.length; i++) {
				Field f = flds[i];
				Object o = f.get(this);
				if(o == null) continue;
				if((o instanceof Boolean) && o.equals(false)) continue;
				if(f.getName().equals("body")) 
					b = o.toString();
				else if(f.getName().equals("enclosed"))
					if(o instanceof AbstractMessage) 
						e = ((AbstractMessage)o).toXml();
					else
						e = o.toString();
				else
					writer.writeAttribute(f.getName(),	o.toString());
			}
			writer.writeStartElement("e:body");
			writer.writeCharacters(b);
			writer.writeEndElement();
			writer.writeStartElement("e:enclosed");
			writer.writeCharacters(e);
			writer.writeEndElement();
			
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
