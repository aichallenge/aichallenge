package com.aicontest.visualizer.js.dom;

import org.w3c.dom.DOMException;

public class HTMLDivElement extends HTMLElement
  implements org.w3c.dom.html.HTMLDivElement
{
  public String getAlign()
  {
    throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
  }

  public void setAlign(String align)
  {
    throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
  }
}