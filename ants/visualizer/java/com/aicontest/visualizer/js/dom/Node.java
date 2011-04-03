package com.aicontest.visualizer.js.dom;

import java.util.Iterator;
import java.util.LinkedList;

import org.w3c.dom.DOMException;
import org.w3c.dom.Document;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.UserDataHandler;

public class Node implements org.w3c.dom.Node {
	private NodeList childNodes;
	private Node parentNode;

	public Node() {
		childNodes = new NodeList();
	}

	void contentChanged() {
		if (parentNode != null)
			parentNode.contentChanged();
	}

	public String getNodeName() {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public String getNodeValue() throws DOMException {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public void setNodeValue(String nodeValue) throws DOMException {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public short getNodeType() {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public org.w3c.dom.Node getParentNode() {
		return parentNode;
	}

	public org.w3c.dom.NodeList getChildNodes() {
		return childNodes;
	}

	public org.w3c.dom.Node getFirstChild() {
		return (org.w3c.dom.Node) childNodes.nodes.getFirst();
	}

	public org.w3c.dom.Node getLastChild() {
		return (org.w3c.dom.Node) childNodes.nodes.getLast();
	}

	public org.w3c.dom.Node getPreviousSibling() {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public org.w3c.dom.Node getNextSibling() {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public NamedNodeMap getAttributes() {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public Document getOwnerDocument() {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public org.w3c.dom.Node insertBefore(org.w3c.dom.Node newChild,
			org.w3c.dom.Node refChild) throws DOMException {
		int i = 0;
		Iterator<Node> it = childNodes.nodes.iterator();
		while (it.hasNext()) {
			if (it.next() == refChild) {
				childNodes.nodes.add(i, (Node) newChild);
				((Node) newChild).setParentNode(this);
				return newChild;
			}
			i++;
		}
		childNodes.nodes.add((Node) newChild);
		((Node) newChild).setParentNode(this);
		return newChild;
	}

	public org.w3c.dom.Node replaceChild(org.w3c.dom.Node newChild,
			org.w3c.dom.Node oldChild) throws DOMException {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public org.w3c.dom.Node removeChild(org.w3c.dom.Node oldChild)
			throws DOMException {
		childNodes.nodes.remove(oldChild);
		((Node) oldChild).setParentNode(this);
		return oldChild;
	}

	public org.w3c.dom.Node appendChild(org.w3c.dom.Node newChild)
			throws DOMException {
		childNodes.nodes.addLast((Node) newChild);
		((Node) newChild).setParentNode(this);
		return newChild;
	}

	protected void setParentNode(Node parentNode) {
		this.parentNode = parentNode;
		parentNode.childAdded(this);
	}

	protected void childAdded(Node child) {
	}

	public boolean hasChildNodes() {
		return childNodes.getLength() != 0;
	}

	public org.w3c.dom.Node cloneNode(boolean deep) {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public void normalize() {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public boolean isSupported(String feature, String version) {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public String getNamespaceURI() {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public String getPrefix() {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public void setPrefix(String prefix) throws DOMException {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public String getLocalName() {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public boolean hasAttributes() {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public String getBaseURI() {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public short compareDocumentPosition(org.w3c.dom.Node other)
			throws DOMException {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public String getTextContent() throws DOMException {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public void setTextContent(String textContent) throws DOMException {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public boolean isSameNode(org.w3c.dom.Node other) {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public String lookupPrefix(String namespaceURI) {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public boolean isDefaultNamespace(String namespaceURI) {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public String lookupNamespaceURI(String prefix) {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public boolean isEqualNode(org.w3c.dom.Node arg) {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public Object getFeature(String feature, String version) {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public Object setUserData(String key, Object data, UserDataHandler handler) {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	public Object getUserData(String key) {
		throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
	}

	class NodeList implements org.w3c.dom.NodeList {
		private LinkedList<Node> nodes;

		public NodeList() {
			nodes = new LinkedList<Node>();
		}

		public Node item(int index) {
			try {
				return (Node) nodes.get(index);
			} catch (IndexOutOfBoundsException e) {
			}
			throw new DOMException(DOMException.INDEX_SIZE_ERR, "Index "
					+ index + " was requested, while we only have "
					+ nodes.size() + " nodes.");
		}

		public int getLength() {
			return nodes.size();
		}
	}

}