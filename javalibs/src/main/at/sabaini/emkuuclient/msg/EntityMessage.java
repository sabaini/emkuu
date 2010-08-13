package at.sabaini.emkuuclient.msg;

import java.lang.reflect.Field;

import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;

public abstract class EntityMessage extends AbstractMessage {
	public String body;
	public boolean fragment;
	public Long contentlen;
	public String contenttype;

	protected void writeAttrs(XMLStreamWriter writer) {
		Field[] flds = this.getClass().getFields();
		try {
			String b = "";
			for(int i=0; i<flds.length; i++) {
				Field f = flds[i];
				Object o = f.get(this);
				if(o == null) continue;
				if((o instanceof Boolean) && o.equals(false)) continue;
				if(f.getName().equals("body")) 
					b = o.toString();
				else
					writer.writeAttribute(f.getName(),	o.toString());
			}
			writer.writeCharacters(b);
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
