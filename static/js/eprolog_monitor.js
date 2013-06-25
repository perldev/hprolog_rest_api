var vm_statistic = {}

var timerId

var arrey = []

vm_statistic.ws = new WebSocket("ws://localhost:8313/websocket");

vm_statistic.directives = {}

vm_statistic.ws.onopen = function(evt){
    console.log("Socket open");
}

vm_statistic.ws.onclose = function(){ 
    console.log("Socket closed");  
}

vm_statistic.events = {}


/*vm_statistic.directives.vmstatus_log = $p('#vmstatus_log-page').compile(
{
    '@class+':' active',
    'table':{
        'node<-':{
            'caption[data-designation="node_name_log"]':function(nodename_log){return nodename_log.node.pos},
            'table tbody tr':{
                'row<-node':{
                    'td[data-designation="date"]':'row.date',
                    'td[data-designation="total"]':'row.total',
                    'td[data-designation="processes"]':'row.processes',
                    'td[data-designation="processes_used"]':'row.processes_used',
                    'td[data-designation="system"]':'row.system',
                    'td[data-designation="atom"]':'row.atom',
                    'td[data-designation="atom_used"]':'row.atom_used',
                    'td[data-designation="binary"]':'row.binary',
			        'td[data-designation="code"]':'row.code',
                    'td[data-designation="ets"]':'row.ets',
                    'td[data-designation="process_count"]':'row.process_count',
                    'td[data-designation="process_limit"]':'row.process_limit'
                }
            }
                }
            }
})	*/


vm_statistic.events.pageChange = function(evt){
    var page = $(this).attr('data-page')
    switch (page)
    {
        case "requests_to_work":
            clearInterval(timerId);
            timerId = setInterval('vm_statistic.ws.send(JSON.stringify({action: "get", page: page}))', 3000);
        case "processes":
            clearInterval(timerId);
            timerId = setInterval('vm_statistic.ws.send(JSON.stringify({action: "get", page: page}))', 3000);
        case "show_code":
            clearInterval(timerId);
            vm_statistic.ws.send(JSON.stringify({action: "get", page: page}));
        case "undefined":
        break
    }  
}

vm_statistic.events.pageAutoChange = function(evt)
{
    page = $(this).attr('data-pageAutoChange')
    switch (page)
    {
        case "statistic":
            timerId = setInterval('vm_statistic.ws.send(JSON.stringify({action: "get", page: "statistic"}))', 3000);
        break
    }
}


$('a').on('click', vm_statistic.events.pageAutoChange)
$('a').on('click', vm_statistic.events.pageChange)

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
	        switch (msg.page){
                case "vmstatus_log":
                    vm_statistic.page = msg.page
                    var selectorId = '#' + vm_statistic.page +'-page'
                    $(selectorId).render(msg.data, vm_statistic.directives[vm_statistic.page])
                    break	            
                case "statistic":
                    //var arrey = msg.statistic
                    var arrey = [
                                [Math.floor((Math.random()*100)+1), Math.floor((Math.random()*100)+1), Math.floor((Math.random()*100)+1)],
                                [Math.floor((Math.random()*100)+1), Math.floor((Math.random()*100)+1), Math.floor((Math.random()*100)+1)],
                                [Math.floor((Math.random()*100)+1), Math.floor((Math.random()*100)+1), Math.floor((Math.random()*100)+1)],
                                [Math.floor((Math.random()*100)+1), Math.floor((Math.random()*100)+1), Math.floor((Math.random()*100)+1)],
                                [Math.floor((Math.random()*100)+1), Math.floor((Math.random()*100)+1), Math.floor((Math.random()*100)+1)],
                                [Math.floor((Math.random()*100)+1), Math.floor((Math.random()*100)+1), Math.floor((Math.random()*100)+1)],
                                [Math.floor((Math.random()*100)+1), Math.floor((Math.random()*100)+1), Math.floor((Math.random()*100)+1)],
                                [Math.floor((Math.random()*100)+1), Math.floor((Math.random()*100)+1), Math.floor((Math.random()*100)+1)],
                                [Math.floor((Math.random()*100)+1), Math.floor((Math.random()*100)+1), Math.floor((Math.random()*100)+1)],
                                [Math.floor((Math.random()*100)+1), Math.floor((Math.random()*100)+1), Math.floor((Math.random()*100)+1)],
                                [Math.floor((Math.random()*100)+1), Math.floor((Math.random()*100)+1), Math.floor((Math.random()*100)+1)],
                                [Math.floor((Math.random()*100)+1), Math.floor((Math.random()*100)+1), Math.floor((Math.random()*100)+1)],
                                [Math.floor((Math.random()*100)+1), Math.floor((Math.random()*100)+1), Math.floor((Math.random()*100)+1)],
                                [Math.floor((Math.random()*100)+1), Math.floor((Math.random()*100)+1), Math.floor((Math.random()*100)+1)],
                                [Math.floor((Math.random()*100)+1), Math.floor((Math.random()*100)+1), Math.floor((Math.random()*100)+1)],
                                [Math.floor((Math.random()*100)+1), Math.floor((Math.random()*100)+1), Math.floor((Math.random()*100)+1)],
                                [Math.floor((Math.random()*100)+1), Math.floor((Math.random()*100)+1), Math.floor((Math.random()*100)+1)],
                                [Math.floor((Math.random()*100)+1), Math.floor((Math.random()*100)+1), Math.floor((Math.random()*100)+1)],
                                [Math.floor((Math.random()*100)+1), Math.floor((Math.random()*100)+1), Math.floor((Math.random()*100)+1)],
                                [Math.floor((Math.random()*100)+1), Math.floor((Math.random()*100)+1), Math.floor((Math.random()*100)+1)],
                                [Math.floor((Math.random()*100)+1), Math.floor((Math.random()*100)+1), Math.floor((Math.random()*100)+1)],
                                [Math.floor((Math.random()*100)+1), Math.floor((Math.random()*100)+1), Math.floor((Math.random()*100)+1)],
                                [Math.floor((Math.random()*100)+1), Math.floor((Math.random()*100)+1), Math.floor((Math.random()*100)+1)],
                                [Math.floor((Math.random()*100)+1), Math.floor((Math.random()*100)+1), Math.floor((Math.random()*100)+1)],
                                [Math.floor((Math.random()*100)+1), Math.floor((Math.random()*100)+1), Math.floor((Math.random()*100)+1)],
                                [Math.floor((Math.random()*100)+1), Math.floor((Math.random()*100)+1), Math.floor((Math.random()*100)+1)],
                                [Math.floor((Math.random()*100)+1), Math.floor((Math.random()*100)+1), Math.floor((Math.random()*100)+1)],
                                [Math.floor((Math.random()*100)+1), Math.floor((Math.random()*100)+1), Math.floor((Math.random()*100)+1)],
                                [Math.floor((Math.random()*100)+1), Math.floor((Math.random()*100)+1), Math.floor((Math.random()*100)+1)],
                                [Math.floor((Math.random()*100)+1), Math.floor((Math.random()*100)+1), Math.floor((Math.random()*100)+1)],
                                [Math.floor((Math.random()*100)+1), Math.floor((Math.random()*100)+1), Math.floor((Math.random()*100)+1)]
                    ]
                    //console.log("arrey:", arrey);
                    $.map( arrey, function(point){
                    chart.series[0].addPoint(point, true, true);
                    });
                    case "requests_to_work":
                    //var requests = msg.requests
                    var requests = "[{<0.18222.454>,{assert,{user_ip,"2603018974","195.211.175.165"}}}]",
                    console.log("requests:", requests),
                    $("#requests").append("<br/>");
                    $("#requests").append("<div>" + requests + "</div>");
                break
        }
    } 
}


