package com.aicontest.visualizer.js.dom;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.HttpURLConnection;
import java.net.MalformedURLException;
import java.net.ProtocolException;
import java.net.URL;
import java.net.URLConnection;
import java.util.zip.GZIPInputStream;

import org.w3c.dom.DOMException;

import com.aicontest.visualizer.WebWrapper;
import com.aicontest.visualizer.js.tasks.EventExecutionUnit;

public class XMLHttpRequest {

	public Object onreadystatechange;
	static final short UNSENT = 0;
	static final short OPENED = 1;
	static final short HEADERS_RECEIVED = 2;
	static final short LOADING = 3;
	static final short DONE = 4;
	private short readyState = 0;
	private URLConnection conn;
	private String statusText;
	String responseText;
	private Document responseXML;

	public short getReadyState() {
		return readyState;
	}

	public void open(String method, String url) {
		open(method, url, true);
	}

	public void open(String method, String url, boolean async) {
		open(method, url, async, null);
	}

	public void open(String method, String url, boolean async, String user) {
		open(method, url, async, user, null);
	}

	public void open(String method, String url, boolean async, String user, String password) {
		method = method.toUpperCase();
		if ((!"CONNECT".equals(method)) && (!"DELETE".equals(method)) && (!"GET".equals(method)) && (!"HEAD".equals(method)) && (!"OPTIONS".equals(method)) && (!"POST".equals(method)) && (!"PUT".equals(method)) && (!"TRACE".equals(method)) && (!"TRACK".equals(method))) {
			throw new DOMException(DOMException.SYNTAX_ERR, method + " is not a valid http method");
		}
		if (("CONNECT".equals(method)) || ("TRACE".equals(method)) || ("TRACK".equals(method))) {
			throw new SecurityException(method + " is not allowed.");
		}
		WebWrapper webWrapper = WebWrapper.getInstance();
		try {
			URL baseURL = webWrapper.getBaseURL();
			try {
				URL urlObject = new URL(baseURL, url);
				conn = urlObject.openConnection();
			} catch (MalformedURLException e) {
				throw new DOMException(DOMException.SYNTAX_ERR, url + " can not be resolved in the context of " + baseURL);
			}
		} catch (Exception e) {
			throw new DOMException(DOMException.INVALID_STATE_ERR, "internal error: " + e.getMessage());
		}
		abortSend();
		try {
			if ((conn instanceof HttpURLConnection))
				((HttpURLConnection) conn).setRequestMethod(method);
		} catch (ProtocolException e) {
			throw new DOMException(DOMException.SYNTAX_ERR, e.getMessage());
		}
		readyState = 1;
	}

	private void abortSend() {}

	public void setRequestHeader(String header, String value) {
		conn.addRequestProperty(header, value);
	}

	public void send() throws Exception {
		InputStream is = conn.getInputStream();
		if ("x-gzip".equals(conn.getContentEncoding())) {
			is = new GZIPInputStream(is);
		}
		ByteArrayOutputStream bos = new ByteArrayOutputStream();
		int read;
		byte[] bytes = new byte[256 * 256];
		do {
			read = is.read(bytes);
			if (read > 0) {
				bos.write(bytes, 0, read);
			}
		} while (read > 0);
		responseText = bos.toString();
		readyState = 4;
		WebWrapper.getInstance().addTask(new EventExecutionUnit(this, "onreadystatechange", new Object[0]));
	}

	public void send(Document data) {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public void send(String data) {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public void abort() {
		if (conn != null) {
			if ((conn instanceof HttpURLConnection)) {
				((HttpURLConnection) conn).disconnect();
			}
			conn = null;
		}
	}

	public short getStatus() throws IOException {
		if ((conn instanceof HttpURLConnection)) {
			return (short) ((HttpURLConnection) conn).getResponseCode();
		}
		return 200;
	}

	public String getStatusText() {
		return statusText;
	}

	public String getResponseHeader(String header) {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public String getAllResponseHeaders() {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public String getResponseText() {
		return responseText;
	}

	public Document getResponseXML() {
		return responseXML;
	}
}
