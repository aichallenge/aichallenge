package com.aicontest.visualizer.js.dom;

import org.w3c.dom.DOMException;

public class HTMLElement extends Element
  implements org.w3c.dom.html.HTMLElement
{
  private String innerHTML;
  private CSS2Properties style;

  public HTMLElement()
  {
    innerHTML = "";
    style = new CSS2Properties(this);
  }

  public String getId()
  {
    throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
  }

  public void setId(String id)
  {
    throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
  }

  public String getTitle()
  {
    throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
  }

  public void setTitle(String title)
  {
    throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
  }

  public String getLang()
  {
    throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
  }

  public void setLang(String lang)
  {
    throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
  }

  public String getDir()
  {
    throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
  }

  public void setDir(String dir)
  {
    throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
  }

  public String getClassName()
  {
    throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
  }

  public void setClassName(String className)
  {
    throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
  }

  public String getInnerHTML() {
    return innerHTML;
  }

  public void setInnerHTML(String text) {
    innerHTML = text;
  }

  public CSS2Properties getStyle() {
    return style;
  }
}