function hideLines()
{
    var z = document.evaluate("//*[name() = 'g' and namespace-uri() = 'http://www.w3.org/2000/svg' and (@class = 'points' or @class = 'lines')]", document, null, XPathResult.UNORDERED_NODE_SNAPSHOT_TYPE);

    var g;
    var ctr = 0;
    while(ctr < z.snapshotLength) {
	g = z.snapshotItem(ctr++);
	console.log('hiding ' + g + " " + g.getAttribute("display")) ;
//	g.setAttribute("visiblility", "hidden");
	if(g.getAttribute("display") == "none")
	    g.setAttribute("display", "inline");
	else
	    g.setAttribute("display", "none");		    
    }

}

