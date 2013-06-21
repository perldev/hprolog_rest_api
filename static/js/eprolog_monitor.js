var vm_statistic = {}

vm_statistic.ws = new WebSocket("ws://localhost:8313/websocket");

vm_statistic.directives = {}

vm_statistic.ws.onopen = function(evt)
{
    console.log("Socket open")
    timer_vmstatus_log = setInterval('vm_statistic.ws.send(JSON.stringify({action: "get", page: "statistic"}))', 3000)
}

vm_statistic.events = {}

vm_statistic.ws.onclose = function()
{ 
    console.log("Socket closed"); 
}

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

vm_statistic.events.pageChange = function(evt)
{
    var page = $(this).attr('data-page')
    if (page)
    {
        vm_statistic.ws.send(JSON.stringify({action: "get", page: page}))
        switch (typeof timer_vmstatus_log)
        {
            case "number":
                clearInterval(timer_vmstatus_log)
            break
            case "undefined":
            break
        }    
    }
    return true
}

/*
vm_statistic.events.pageAutoChange = function(evt)
{
    page = $(this).attr('data-pageAutoChange')
    switch (page)
    {
        case "vmstatus_live":
            timer_vmstatus_log = setInterval('vm_statistic.ws.send(JSON.stringify({action: "statistic", page: "vmstatus_live"}))', 3000)
        break
    }
}   */

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
	    series: [{
	        name: 'Fact1',  
            data: []
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
                    time = (new Date()).getTime(),
                    i;
                    for (i = -19; i <= 0; i++) {
                        data.push({
                            x: time + i * 1000,
                            y: Math.random()
                        });
                    }
                    return data;
                }

function eventData(){
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
                    var arrey = msg.statistic
                    console.log("arrey:", arrey);
                    $.map( arrey, function(n){
                    chart.series[0].addPoint(n);
                    });
        }
    } 
}


