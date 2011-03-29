package com.aicontest.visualizer.js.dom;

import com.aicontest.visualizer.js.WebWrapper;
import java.applet.Applet;
import java.awt.Container;
import java.awt.Frame;
import netscape.javascript.JSObject;
import org.w3c.dom.DOMException;
import org.w3c.dom.html.HTMLCollection;
import org.w3c.dom.html.HTMLElement;

public class HTMLDocument extends Document
  implements org.w3c.dom.html.HTMLDocument
{
  public Object onkeydown;
  public Object onkeyup;
  public Object onkeypress;

  public HTMLDocument()
  {
    super(WebWrapper.getInstance().getContainer());
  }

  public String getTitle()
  {
    Container frame = WebWrapper.getInstance().getContainer();
    if ((frame instanceof Frame))
      return ((Frame)frame).getTitle();
    if ((frame instanceof Applet)) {
      ((JSObject)JSObject.getWindow((Applet)frame).getMember("document")).getMember("title");
    }
    return "";
  }

  public void setTitle(String title)
  {
    Container frame = WebWrapper.getInstance().getContainer();
    if ((frame instanceof Frame))
      ((Frame)frame).setTitle(title);
    else if ((frame instanceof Applet))
      ((JSObject)JSObject.getWindow((Applet)frame).getMember("document")).setMember("title", title);
  }

  public String getReferrer()
  {
    throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
  }

  public String getDomain()
  {
    throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
  }

  public String getURL()
  {
    throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
  }

  public HTMLElement getBody()
  {
    throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
  }

  public void setBody(HTMLElement body)
  {
    throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
  }

  public HTMLCollection getImages()
  {
    throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
  }

  public HTMLCollection getApplets()
  {
    throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
  }

  public HTMLCollection getLinks()
  {
    throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
  }

  public HTMLCollection getForms()
  {
    throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
  }

  public HTMLCollection getAnchors()
  {
    throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
  }

  public String getCookie()
  {
    throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
  }

  public void setCookie(String cookie)
  {
    throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
  }

  public void open()
  {
    throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
  }

  public void close()
  {
    throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
  }

  public void write(String text)
  {
    throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
  }

  public void writeln(String text)
  {
    throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
  }

  public Node.NodeList getElementsByName(String elementName)
  {
    throw new DOMException(DOMException.NOT_SUPPORTED_ERR, "not supported");
  }

  void contentChanged()
  {
    WebWrapper.getInstance().getDomWindow().contentChanged();
  }

  protected void childAdded(Node child)
  {
    super.childAdded(child);
    if ((child instanceof HTMLCanvasElement))
      WebWrapper.getInstance().setCanvas((HTMLCanvasElement)child);
  }
}