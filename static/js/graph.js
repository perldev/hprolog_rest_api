"use strict";


var vm_statistic = {
    page: null,
    timerId: null,
    namespace: null,
    options : {
                title: 'Facts bubbles',
                hAxis: {title: 'Popularity', maxValue: 1000000, minValue: -100000  },
                vAxis: {title: 'Weight', maxValue: 50, minValue:-10 },
                width:1024,
                height:400,
                bubble: {textStyle: {fontSize: 11}},
                colorAxis: {colors: ['yellow', 'blue']}
            },
    change_namespace: function(Key, Session){
        if(Key != vm_statistic.namespace){
            vm_statistic.ws.send(JSON.stringify({action: "change", cmd: "status", namespace: Key, token: Session }));     
            vm_statistic.namespace = Key;
            clearInterval(vm_statistic.timerId);
            $('ul.nav li').removeClass('active');

        }
        
    },
    events: 
    {
        pageChange : function(evt){
                            
            
                            var Page = $(this).attr('id');
                            if (Page == vm_statistic.page  )
                                    return ;
                            
                            vm_statistic.page = Page;
                            vm_statistic.emptyInstance();
                            clearInterval(vm_statistic.timerId);
                            vm_statistic.switchSidebars(vm_statistic.page);
                            $('ul.nav li').removeClass('active');
                            $(this).parent().attr('class', 'active');

                            switch (vm_statistic.page) {
                                case "graph":
                                    $("#graph_container").show(); 
                                    console.log("load graph_page");
                                    break;
                                case "system_state":
                                    $("#system_state_container").show(); 
                                    console.log("load system_state");
                                    break;
                            case "code_memory":
                                    $("#code_memory_container").show();
                                   console.log("memory container");
                                   break;
                            case "requests":
                                 $("#requests_container").show();
                                 console.log("memory container");
                                 break; 
                            }  
                        },
    },
   eventData: function  (){
            var requests = [], namespaces = [], system_data = []; 
            var arrey = [], GraphData = [], code_memory_data = [];
            vm_statistic.ws.onmessage = function(evt) { 
                        console.log("Received_msg:", evt.data);
                        var msg = JSON.parse(evt.data);
                        console.log("cmd:", msg.cmd);
                        switch (msg.cmd){
                                case "graph":
                                    console.log("process:", msg);
                    
                                    var Array = msg.graph_data;
                                    vm_statistic.drawChart(Array);
                                    vm_statistic.unblock();
                                    clearInterval(vm_statistic.timerId);
                                    break;
                                case "requests":
                                     console.log("process:", msg);
                                    $("#requests_container").empty();
                                    var requests = msg.requests;
                                        console.log("requests:", requests);
                                        $("#requests_container").append("<br/>");
                                    $("#requests_container").append("<div> Number Requests: " + msg.count + "</div>");
                                        $.map(requests, function(req1){
                                        $("#requests_container").append("<br/>");
                                        $("#requests_container").append("<div>" + req1.request + "</div>"); 
                                        });
                                    break;
                                case "system_state":
                                    console.log("process:", msg);
                                    var system_data = msg.system_state;
                                    $("#system_state_container").empty();
                                        $("#system_state_container").append("<br/>");
                                    $("#system_state_container").append("<div> Number Processes: "+ msg.processes + " Memory/mb: " + msg.memory + "</div>");
                                        $("#system_state_container").append("<br/>");
                                    $("#system_state_container").append("<div> Number Requests: " + msg.count + "</div>");
                                    var  requests = msg.system_state;
                                        $.map(requests, function(req1){
                                        $("#system_state_container").append("<br/>");
                                        $("#system_state_container").append("<div>" + req1.state + "</div>");
                                        });
                                    break;
                                case "code_memory":
                                    console.log("process:", msg);
                                    var code_memory_data = msg.code_memory;
                                    $("#code_memory_container").empty();
                                    $("#code_memory_container").append("<br/>");
                                    $("#code_memory_container").append("<div>" + code_memory_data + "</div>");
                                    break;
                        }    
                } 
     },
    block: function(){
     $("#block").show()
    
    },
    unblock: function(){
     $("#block").hide('fast')
    
    },
    switchSidebars: function(sideBarId){
        vm_statistic.timerId = setInterval(function(){
                                                            vm_statistic.ws.send(JSON.stringify({action: "get", cmd: sideBarId}))
                                                        }
                                                        , 1000);
            
    },
    drawChart: function(Array){
//     [
//           ['ID', 'Popularity', 'Weight', 'Seria',     'Facts Count'],
//           ['CAN',    80.66,              1.67,      'North America',  33739900],
//           ['DEU',    79.84,              1.36,      'Europe',         81902307],
//         ]);

    
    
//     graph        
       var ResArray = [  ['ID', 'Popularity', 'Weight', 'Seria',     'Facts Count'] ];
       $.map( Array, function(Point){
                    var NewArray = [ '', Point.data[0], Point.data[1],  Point.name, Point.data[2] ];
                    ResArray.push(NewArray);
       });
       var data = google.visualization.arrayToDataTable(ResArray );   
       var chart = new google.visualization.BubbleChart(document.getElementById('graph_container'));
       chart.draw(data, this.options);      
    

    },
    emptyInstance: function(){
        $("#requests_container").empty();
        $("#system_state_container").empty();
        $("#code_memory_container").empty();
        $("#graph_container").empty();

    }
           
};







 








