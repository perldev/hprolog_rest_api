var vm_statistic = {};

vm_statistic.events = {};

var FactName = "Init";

var timerId;

var page = "graph";

var sideBarId;

// need change host
vm_statistic.ws = new WebSocket("ws://avias-db-2.ceb.loc:8313/websocket");

//vm_statistic.ws = new WebSocket("ws://localhost:8313/websocket");

vm_statistic.ws.onopen = function(evt){
    console.log("Socket open");
    $("#monitor_menu").hide();
    vm_statistic.ws.send(JSON.stringify({action: "get", cmd: "namespaces"}));
}

$(document).ready(function() {
    chart = new Highcharts.Chart(options);
});

vm_statistic.ws.onclose = function(){
    console.log("Socket closed"); 
}

vm_statistic.events.pageChange = function(evt){
    page = $(this).attr('data-page');
    switch (page) {
	case "graph":
        emptyInstance();
        clearInterval(timerId);
        $("#monitor_menu").hide();
        $("#namespaces").show();
        $("#graph").show(); 
	    console.log("load graph_page");
	break
    case "monitor":
        emptyInstance();
        clearInterval(timerId);
        $("#graph").hide();
        $("#namespaces").show();
        $("#monitor_menu").show();
        sideBarId = "requests"; 
        console.log("load monitor_page");
    break
    case "sidebar":
        emptyInstance();
        clearInterval(timerId);
        sideBarId = $(this).attr('id');
        $('li').removeClass('active');
        $(this).parent().attr('class', 'active');
        switchSidebars(sideBarId);
    }  
}

vm_statistic.events.buttonChange = function(evt){
    var state = $(this).attr('data-button');
    vm_statistic.ws.send(JSON.stringify({action: "change", cmd: "status", value: state}));
}

$('a').on('click', vm_statistic.events.pageChange);
$("a[id='button']").on('click', vm_statistic.events.buttonChange);

function switchSidebars(){
    console.log("sideBarId:", sideBarId);
    switch (sideBarId){
        case "requests":
            $("#namespaces").show();
        break
        case "code_memory":
            $("#namespaces").show();
        break
        case "system_state":
            $("#namespaces").hide();
            timerId = setInterval('vm_statistic.ws.send(JSON.stringify({action: "get", cmd: sideBarId}))', 3000);
        break   
    }
}

 
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
	    series: [{name: FactName, data: genMassive()
	}]
}

// generate an array of random data to graph 1000 points
function genMassive()  {
                    var data = [],
                    time = (new Date()).getTime(),i;
                    for (i = -50; i <= 0; i++) {
                        data.push({
                            x: Math.floor((Math.random()*100)+1),
                            y: Math.floor((Math.random()*100)+1),
                            z: Math.floor((Math.random()*100)+1),
                        });
                    }
                    return data;
}

function eventData(){
    var requests = [], namespaces = [], system_data = []; 
    var arrey = [], GraphData = [], code_memory_data = [];
    var series = this.series[0];
    vm_statistic.ws.onmessage = function(evt) { 
	console.log("Received_msg:", evt.data);
    var msg = JSON.parse(evt.data);
	console.log("cmd:", msg.cmd);
	switch (msg.cmd){
	    case "namespaces":
		    namespaces = msg.namespaces;
		    console.log("namespaces:", namespaces);
		    appendNamespaces(namespaces);
	    break
        case "graph":
            arrey = msg.graph_data;
            $.map( arrey, function(Point){
		    //FactName = Point.name;
		    GraphData = Point.data;
            chart.series[0].addPoint(GraphData, true, true);
            });
	    break
        case "requests":
            $("#requests_container").empty();
            requests = msg.requests;
	        console.log("requests:", requests);
	        $("#requests_container").append("<br/>");
            $("#requests_container").append("<div> Number Requests: " + msg.count + "</div>");
	        $.map(requests, function(req1){
	        $("#requests_container").append("<br/>");
	        $("#requests_container").append("<div>" + req1.request + "</div>"); 
	        });
	    break
        case "system_state":
            system_data = msg.system_state;
            $("#system_state_container").empty();
	        $("#system_state_container").append("<br/>");
            $("#system_state_container").append("<div> Number Processes: "+ msg.processes + " Memory/mb: " + msg.memory + "</div>");
	        $("#system_state_container").append("<br/>");
            $("#system_state_container").append("<div> Number Requests: " + msg.count + "</div>");
	        requests = msg.system_state;
	        $.map(requests, function(req1){
		    $("#system_state_container").append("<br/>");
            $("#system_state_container").append("<div>" + req1.state + "</div>");
 	        });
        break
        case "code_memory":
            code_memory_data = msg.code_memory;
            $("#code_memory_container").empty();
            $("#code_memory_container").append("<br/>");
            $("#code_memory_container").append("<div>" + code_memory_data + "</div>");
        break
        }    
    } 
}

function emptyInstance(){
    $("#requests_container").empty();
    $("#system_state_container").empty();
    $("#code_memory_container").empty();
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
    switch(page){
    case "graph":
        console.log("graph_page");
    	clearInterval(timerId);
    	timerId = setInterval('vm_statistic.ws.send(JSON.stringify({action: "get", namespace: NameSpace, cmd: page}))', 30000);
    break
    case "monitor":
	    console.log("requests_page");
    case "sidebar":
        switch (sideBarId){
        case "requests":
            clearInterval(timerId);
            timerId = setInterval('vm_statistic.ws.send(JSON.stringify({action: "get", namespace: NameSpace, cmd: sideBarId}))', 3000);
        break
        case "code_memory":
            clearInterval(timerId);
            vm_statistic.ws.send(JSON.stringify({action: "get", namespace: NameSpace, cmd: sideBarId}))
        break

        }
    } 
}



