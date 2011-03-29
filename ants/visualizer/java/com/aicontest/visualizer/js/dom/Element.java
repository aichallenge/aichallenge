package com.aicontest.visualizer.js.dom;

import org.w3c.dom.Attr;
import org.w3c.dom.DOMException;
import org.w3c.dom.TypeInfo;

public class Element extends Node
  implements org.w3c.dom.Element
{
  public String getTagName()
  {
    throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
  }

  public String getAttribute(String name)
  {
    throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
  }

  public void setAttribute(String name, String value) throws DOMException
  {
    throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
  }

  public void removeAttribute(String name) throws DOMException
  {
    throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
  }

  public Attr getAttributeNode(String name)
  {
    throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
  }

  public Attr setAttributeNode(Attr newAttr) throws DOMException
  {
    throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
  }

  public Attr removeAttributeNode(Attr oldAttr) throws DOMException
  {
    throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
  }

  public Node.NodeList getElementsByTagName(String name)
  {
    throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
  }

  public String getAttributeNS(String namespaceURI, String localName)
    throws DOMException
  {
    throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
  }

  public void setAttributeNS(String namespaceURI, String qualifiedName, String value)
    throws DOMException
  {
    throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
  }

  public void removeAttributeNS(String namespaceURI, String localName)
    throws DOMException
  {
    throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
  }

  public Attr getAttributeNodeNS(String namespaceURI, String localName)
    throws DOMException
  {
    throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
  }

  public Attr setAttributeNodeNS(Attr newAttr) throws DOMException
  {
    throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
  }

  public Node.NodeList getElementsByTagNameNS(String namespaceURI, String localName)
    throws DOMException
  {
    throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
  }

  public boolean hasAttribute(String name)
  {
    throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
  }

  public boolean hasAttributeNS(String namespaceURI, String localName)
    throws DOMException
  {
    throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
  }

  public TypeInfo getSchemaTypeInfo()
  {
    throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
  }

  public void setIdAttribute(String name, boolean isId) throws DOMException
  {
    throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
  }

  public void setIdAttributeNS(String namespaceURI, String localName, boolean isId)
    throws DOMException
  {
    throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
  }

  public void setIdAttributeNode(Attr idAttr, boolean isId)
    throws DOMException
  {
    throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
  }
}