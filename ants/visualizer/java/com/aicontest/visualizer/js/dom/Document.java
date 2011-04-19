package com.aicontest.visualizer.js.dom;

import java.awt.Container;
import org.w3c.dom.Attr;
import org.w3c.dom.CDATASection;
import org.w3c.dom.Comment;
import org.w3c.dom.DOMConfiguration;
import org.w3c.dom.DOMException;
import org.w3c.dom.DOMImplementation;
import org.w3c.dom.DocumentFragment;
import org.w3c.dom.DocumentType;
import org.w3c.dom.Element;
import org.w3c.dom.EntityReference;
import org.w3c.dom.ProcessingInstruction;
import org.w3c.dom.Text;

import com.aicontest.visualizer.Visualizer;

public class Document extends Node implements org.w3c.dom.Document {

	protected Visualizer webWrapper;

	public Document(Visualizer webWrapper) {
		this.webWrapper = webWrapper;
	}

	public DocumentType getDoctype() {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public DOMImplementation getImplementation() {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public Element getDocumentElement() {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public Element createElement(String tagName) throws DOMException {
		if ("div".equals(tagName)) {
			return new HTMLDivElement();
		} else if ("canvas".equals(tagName)) {
			Container container = webWrapper.getContainer();
			return new HTMLCanvasElement(container);
		}
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, tagName);
	}

	public DocumentFragment createDocumentFragment() {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public Text createTextNode(String data) {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public Comment createComment(String data) {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public CDATASection createCDATASection(String data) throws DOMException {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public ProcessingInstruction createProcessingInstruction(String target,
			String data) throws DOMException {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public Attr createAttribute(String name) throws DOMException {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public EntityReference createEntityReference(String name)
			throws DOMException {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public Node.NodeList getElementsByTagName(String tagname) {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public org.w3c.dom.Node importNode(org.w3c.dom.Node importedNode,
			boolean deep) throws DOMException {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public Element createElementNS(String namespaceURI, String qualifiedName)
			throws DOMException {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public Attr createAttributeNS(String namespaceURI, String qualifiedName)
			throws DOMException {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public Node.NodeList getElementsByTagNameNS(String namespaceURI,
			String localName) {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public Element getElementById(String elementId) {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public String getInputEncoding() {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public String getXmlEncoding() {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public boolean getXmlStandalone() {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public void setXmlStandalone(boolean xmlStandalone) throws DOMException {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public String getXmlVersion() {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public void setXmlVersion(String xmlVersion) throws DOMException {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public boolean getStrictErrorChecking() {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public void setStrictErrorChecking(boolean strictErrorChecking) {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public String getDocumentURI() {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public void setDocumentURI(String documentURI) {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public org.w3c.dom.Node adoptNode(org.w3c.dom.Node source)
			throws DOMException {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public DOMConfiguration getDomConfig() {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public void normalizeDocument() {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public org.w3c.dom.Node renameNode(org.w3c.dom.Node n, String namespaceURI,
			String qualifiedName) throws DOMException {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}
}