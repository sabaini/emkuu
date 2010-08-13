package at.sabaini.emkuuclient;

import java.util.ArrayList;
import java.util.List;

public class Uri {
	private String scheme = "emkuu";
	private String cluster = "emkuu";
	private List<String> path = new ArrayList<String>();
	private String qry;
	public String getScheme() {
		return scheme;
	}
	public void setScheme(String scheme) {
		this.scheme = scheme;
	}
	public String getCluster() {
		return cluster;
	}
	public void setCluster(String cluster) {
		this.cluster = cluster;
	}
	public List<String> getPath() {
		return path;
	}
	public void setPath(List<String> path) {
		this.path = path;
	}
	public String getQry() {
		return qry;
	}
	public void setQry(String qry) {
		this.qry = qry;
	}
	public String pathString() {
		StringBuilder buf = new StringBuilder();
		for(String s : this.path) {
			buf.append("/");
			buf.append(s);
		}
		return buf.toString();
	}
	public String toString() {
		StringBuilder buf = new StringBuilder();
		buf.append(this.scheme);
		buf.append("://");
		buf.append(this.cluster);
		buf.append(this.pathString());
		if(this.qry != null){
			buf.append("?");
			buf.append(this.qry);			
		}
		return buf.toString();	
	}
	public static Uri decode(String s) {
		Uri u = new Uri();
		// xxx hacky, 8 is after emkuu://
		String a = s.substring(8);
		String[] b = a.split("\\?");
		if(b.length > 1) {
			u.qry = b[1];
		}
		String[] path = b[0].split("/");
		u.cluster = path[0];
		for(int i=1; i<path.length; i++) {
			u.path.add(path[i]);
		}
		return u;
	}
	public String clusterPath() {
		return "/" + this.cluster + this.pathString();
	}
}
