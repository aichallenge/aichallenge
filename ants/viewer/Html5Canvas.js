function onSilverlightLoad(sender, args) {
    var control = sender.getHost();
    // Stub-in the getContext method
    control.getContext = function(context) {
        return control.Content.HTMLCanvasElement.getContext(context);
    }
    // Run the "onload" code (if present)
    if (control.action) {
        control.action(control);
    }
}

// Standard onError method
function onSilverlightError(sender, args) {
    var appSource = "";
    if (sender != null && sender != 0) {
        appSource = sender.getHost().Source;
    }

    var errorType = args.ErrorType;
    var iErrorCode = args.ErrorCode;

    if (errorType == "ImageError" || errorType == "MediaError") {
        return;
    }

    var errMsg = "Unhandled Error in Silverlight Application " + appSource + "\n";

    errMsg += "Code: " + iErrorCode + "    \n";
    errMsg += "Category: " + errorType + "       \n";
    errMsg += "Message: " + args.ErrorMessage + "     \n";

    if (errorType == "ParserError") {
        errMsg += "File: " + args.xamlFile + "     \n";
        errMsg += "Line: " + args.lineNumber + "     \n";
        errMsg += "Position: " + args.charPosition + "     \n";
    }
    else if (errorType == "RuntimeError") {
        if (args.lineNumber != 0) {
            errMsg += "Line: " + args.lineNumber + "     \n";
            errMsg += "Position: " + args.charPosition + "     \n";
        }
        errMsg += "MethodName: " + args.methodName + "     \n";
    }

    throw new Error(errMsg);
}

var canvasHtmlStart =
    '<object data="data:application/x-silverlight-2," type="application/x-silverlight-2"';
var canvasHtmlEnd =
    '>' +
        '<param name="source" value="Html5Canvas.xap"/>' +
        '<param name="onLoad" value="onSilverlightLoad"/>' +
        '<param name="onError" value="onSilverlightError"/>' +
        '<param name="background" value="Transparent"/>' +
        '<param name="windowless" value="true"/>' +
        '<param name="minRuntimeVersion" value="3.0.40624.0"/>' +
        '<param name="autoUpgrade" value="true"/>' +
        '<a href="http://go.microsoft.com/fwlink/?LinkID=149156&v=3.0.40624.0" style="text-decoration:none">' +
          '<img src="http://go.microsoft.com/fwlink/?LinkId=108181" alt="Get Microsoft Silverlight" style="border-style:none"/>' +
        '</a>' +
    '</object>';

// Method that inserts a <canvas> into the page
function InsertCanvasObject(id, width, height, action) {
    document.write("<div class='canvas'>" + canvasHtmlStart + " id='" + id + "' width='" + width + "' height='" + height + "'" + canvasHtmlEnd + "</div>");
    document.getElementById(id).action = action;
}

// Method that creats a <canvas> instance and returns it
function CreateCanvasObject(width, height, action) {
    var div = document.createElement("div");
    div.innerHTML = canvasHtmlStart + " width='" + width + "' height='" + height + "'" + canvasHtmlEnd;
    var obj = div.firstChild;
    obj.action = action;
    return obj;
}
