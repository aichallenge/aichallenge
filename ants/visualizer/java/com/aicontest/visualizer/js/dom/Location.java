package com.aicontest.visualizer.js.dom;

import com.aicontest.visualizer.js.WebWrapper;
import java.applet.Applet;
import java.awt.Container;
import java.awt.Desktop;
import java.net.URI;
import netscape.javascript.JSObject;

public class Location
{
  private JSObject getJSLocation()
  {
    Container container = WebWrapper.getInstance().getContainer();
    if ((container instanceof Applet)) {
      JSObject window = JSObject.getWindow((Applet)container);
      JSObject document = (JSObject)window.getMember("document");
      return (JSObject)document.getMember("location");
    }
    return null;
  }

  public String getHref() {
    JSObject jsLoc = getJSLocation();
    if (jsLoc == null) {
      return "";
    }
    return jsLoc.getMember("href").toString();
  }

  public void setHref(String href)
  {
    JSObject jsLoc = getJSLocation();
    if (jsLoc == null)
      try {
        Desktop.getDesktop().browse(URI.create(href));
      } catch (Exception e) {
        e.printStackTrace();
      }
    else
      jsLoc.setMember("href", href);
  }
}