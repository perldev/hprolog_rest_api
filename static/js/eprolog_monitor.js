var vm_statistic = {};

var timerId;

var arrey = [];

vm_statistic.ws = new WebSocket("ws://avias-db-2.ceb.loc:8313/websocket");

var page;

vm_statistic.ws.onopen = function(evt){
    console.log("Socket open");
    vm_statistic.ws.send(JSON.stringify({action: "get", cmd: "namespaces"}));
}

vm_statistic.ws.onclose = function(){ 
    console.log("Socket closed");  
}
vm_statistic.events = {}

/*vm_statistic.events.pageChange = function(evt){
    var page = $(this).attr('data-page')
    switch (page) {
        case "requests":
            clearInterval(timerId);
            timerId = setInterval('vm_statistic.ws.send(JSON.stringify({action: "get", cmd: page}))', 3000);
        case "processes":
            clearInterval(timerId);
            timerId = setInterval('vm_statistic.ws.send(JSON.stringify({action: "get", cmd: page}))', 3000);
	case "code_memory":
            clearInterval(timerId);
            vm_statistic.ws.send(JSON.stringify({action: "get", cmd: page}));
        case "undefined":
    	    console.log("undefined");
	break
    }  
} */

/*vm_statistic.events.pageAutoChange = function(evt){
    page = $(this).attr('data-pageAutoChange');
    switch (page) {
        case "graph":
            timerId = setInterval('vm_statistic.ws.send(JSON.stringify({action: "get", namespace: "test", cmd: "graph"}))', 3000);
        break
    }
} */


$('a').on('click', vm_statistic.events.pageAutoChange);
$('a').on('click', vm_statistic.events.pageChange);

$(document).ready(function() {
    chart = new Highcharts.Chart(options);
}); 

var options = { chart: {
            renderTo: 'graph',
	        type: 'bubble',
	        zoomType: 'xy',
            events: { load: eventData}
	    },
        title: {
	    	text: 'Counter Facts'
	    },
	    legend: { enabled: false},
	    series: [{ data: genMassive()
	}]
}

/*var options = { chart: {
            renderTo: 'graph',
            defaultSeriesType: 'spline',
            events: { load: eventData}
        },
        title: {
            style: { color: '#021BF6'},
            text: 'Total'
        },
        xAxis: {
            type: 'datetime',
            tickPixelInterval: 150,
            maxZoom: 20 * 1000
        },
        yAxis: {
            minPadding: 0.2,
            maxPadding: 0.2,
            title: { text: 'Total', margin: 20}
        },
        plotOptions: {
                spline: { color: '#021BF6'}
        },
        legend: { enabled: false},
        series: [{name: 'Total', data: genMassive()}]
}*/

function genMassive()  {
                    // generate an array of random data
                    var data = [],
                    time = (new Date()).getTime(),i;
                    for (i = -19; i <= 0; i++) {
                        data.push({
                            x: Math.floor((Math.random()*100)+1),
                            y: Math.floor((Math.random()*100)+1),
                            z: Math.floor((Math.random()*100)+1),
                        });
                    }
                    return data;
}

function eventData(){
    var arrey = [];
    var series = this.series[0];
    vm_statistic.ws.onmessage = function(evt) { 
	console.log("Received_msg:", evt.data);
        var msg = JSON.parse(evt.data);
	console.log(msg.cmd);
	switch (msg.cmd){
	    case "namespaces":
		var namespaces = msg.namespaces;
		console.log("namespaces:", namespaces);
		appendNamespaces(namespaces);
	    break
            case "graph":
                var arrey = msg.graph_data;
                console.log("graph_data:", arrey);
                $.map( arrey, function(point){
                chart.series[0].addPoint(point, true, true);
                })
	    break
            case "requests":
                //var requests = msg.requests
                var requests = "blabla";
                console.log("requests:", requests);
                $("#requests").append("<br/>");
                $("#requests").append("<div>" + requests + "</div>");
	    break
        }    
    } 
}

function appendNamespaces(NameSpaces){
    Res = "NameSpaces : ";
    for( i in NameSpaces) {
    var Name = NameSpaces[i];
    Res +="<span id='"+ Name +"' class='btn btn-info' style='height:20px' onclick=\"sendName(this)\" >" + Name + "</span>&nbsp;";
    	}  
    $("#namespaces").html(Res);                
}

function sendName(obj){
    NameSpace = obj.id;
    console.log(NameSpace);
    clearInterval(timerId);
    timerId = setInterval('vm_statistic.ws.send(JSON.stringify({action: "get", namespace: NameSpace, cmd: "graph"}))', 3000);
}



