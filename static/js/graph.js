"use strict";


var vm_statistic = {
    page: "graph",
    timerId: null,
    sideBarId: null,  
    options : {
                title: 'Facts bubbles',
                hAxis: {title: 'Popularity', maxValue: 1000000, minValue: -100000  },
                vAxis: {title: 'Weight', maxValue: 50, minValue:-10 },
                width:1024,
                height:400,
                bubble: {textStyle: {fontSize: 11}},
                colorAxis: {colors: ['yellow', 'blue']}
            },
    events: 
    {
        buttonChange : function(evt){
                            var state = $(this).attr('data-button');
                            vm_statistic.ws.send(JSON.stringify({action: "change", cmd: "status", value: state}));
                        },
        pageChange : function(evt){
                            vm_statistic.page = $(this).attr('data-page');
                            vm_statistic.clear_sel_namespace();
                            var sideBarId = vm_statistic.sideBarId;

                            switch (vm_statistic.page) {
                                case "graph":
                                vm_statistic.emptyInstance();
                                clearInterval(vm_statistic.timerId);
                                $("#monitor_menu").hide();
                                $("#hero_unit").hide();
                                $("#namespaces").show();
                                $("#graph").show(); 
                                    console.log("load graph_page");
                                break
                            case "monitor":
                                vm_statistic.emptyInstance();
                                clearInterval();
                                $("#graph").hide();
                                $("#namespaces").show();
                                $("#hero_unit").show();
                                $("#monitor_menu").show();
                                vm_statistic.sideBarId = "requests"; 
                                console.log("load monitor_page");
                            break
                            case "sidebar":
                                vm_statistic.emptyInstance();
                                clearInterval(vm_statistic.timerId);
                                vm_statistic.sideBarId = $(this).attr('id');
                                $('li').removeClass('active');
                                $(this).parent().attr('class', 'active');
                                vm_statistic.switchSidebars(sideBarId);
                            }  
                        }
                        

        
    },

   eventData: function  (){
            var requests = [], namespaces = [], system_data = []; 
            var arrey = [], GraphData = [], code_memory_data = [];
            vm_statistic.ws.onmessage = function(evt) { 
                        console.log("Received_msg:", evt.data);
                        var msg = JSON.parse(evt.data);
                        console.log("cmd:", msg.cmd);
                        switch (msg.cmd){
                            case "namespaces":
                                    var namespaces = msg.namespaces;
                                    console.log("namespaces:", namespaces);
                                    vm_statistic.appendNamespaces(namespaces);
                            break;
                        case "graph":
                            var Array = msg.graph_data;
                            vm_statistic.drawChart(Array);
                            vm_statistic.unblock();
                            break;

                        case "requests":
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
                            var code_memory_data = msg.code_memory;
                            $("#code_memory_container").empty();
                            $("#code_memory_container").append("<br/>");
                            $("#code_memory_container").append("<div>" + code_memory_data + "</div>");
                            break;
                        }    
                } 
     },
    clear_sel_namespace:function(){
            $("span.btn-warning").removeClass("btn-warning").addClass("btn-info")

    },
    block: function(){
     $("#block").show()
    
    },
    unblock: function(){
     $("#block").hide('fast')
    
    },
    switchSidebars: function(){
        var sideBarId = vm_statistic.sideBarId;
        vm_statistic.clear_sel_namespace();
        console.log("sideBarId:", sideBarId);
        switch (sideBarId){
            case "requests":
                $("#namespaces").show();
                break;
            case "code_memory":
                $("#namespaces").show();
                break;
            case "system_state":
                $("#namespaces").hide();
                
                vm_statistic.timerId = setInterval(function(){
                                                            vm_statistic.ws.send(JSON.stringify({action: "get", cmd: vm_statistic.sideBarId}))
                                                        }
                                                        , 3000);
                break;
        }
    },
    drawChart: function(Array){
//     [
//           ['ID', 'Popularity', 'Weight', 'Seria',     'Facts Count'],
//           ['CAN',    80.66,              1.67,      'North America',  33739900],
//           ['DEU',    79.84,              1.36,      'Europe',         81902307],
//           ['DNK',    78.6,               1.84,      'Europe',         5523095],
//           ['EGY',    72.73,              2.78,      'Middle East',    79716203],
//           ['GBR',    80.05,              2,         'Europe',         61801570],
//           ['IRN',    72.49,              1.7,       'Middle East',    73137148],
//           ['IRQ',    68.09,              4.77,      'Middle East',    31090763],
//           ['ISR',    81.55,              2.96,      'Middle East',    7485600],
//           ['RUS',    68.6,               1.54,      'Europe',         141850000],
//           ['USA',    78.09,              2.05,      'North America',  307007000]
//         ]);

    
    
//     graph        
       var ResArray = [  ['ID', 'Popularity', 'Weight', 'Seria',     'Facts Count'] ];
       $.map( Array, function(Point){
                    var NewArray = [ '', Point.data[0], Point.data[1],  Point.name, Point.data[2] ];
                    ResArray.push(NewArray);
       });
       var data = google.visualization.arrayToDataTable(ResArray );   
       var chart = new google.visualization.BubbleChart(document.getElementById('graph'));
       chart.draw(data, this.options);      
    

    },
    emptyInstance: function(){
        $("#requests_container").empty();
        $("#system_state_container").empty();
        $("#code_memory_container").empty();
    },
    appendNamespaces :function (NameSpaces){
            var Res = "NameSpaces : ";
            for(var i in NameSpaces) {
            var Name = NameSpaces[i];
            Res +="<span id='"+ Name +"' class='btn btn-info' style='height:20px' onclick=\"vm_statistic.sendName(this)\" >" + Name + "</span>&nbsp;";
            }  
            $("#namespaces").html(Res);                
     },   
    sendName :function(obj){
        var NameSpace = obj.id;
        vm_statistic.clear_sel_namespace();
        $(obj).removeClass("btn-info");
        $(obj).addClass("btn-warning");
        var sideBarId = vm_statistic.sideBarId;
        var page =  vm_statistic.page;
        switch(page){
        case "graph":
            console.log("graph_page");
            clearInterval(vm_statistic.timerId);
            vm_statistic.block();
            vm_statistic.ws.send(JSON.stringify({action: "get", namespace: NameSpace, cmd: page}));
            break;
        case "monitor":
                console.log("requests_page");
                
        case "sidebar":
            switch (sideBarId){
            case "requests":
                clearInterval(vm_statistic.timerId);
                vm_statistic.timerId = setInterval(function(){
                        
                        vm_statistic.ws.send(JSON.stringify({action: "get", namespace: NameSpace, cmd: sideBarId}))
                    
                        
                }, 3000);
            break;
            case "code_memory":
                clearInterval(vm_statistic.timerId);
                vm_statistic.ws.send(JSON.stringify({action: "get", namespace: NameSpace, cmd: sideBarId}))
            break;

            }
        } 
    }
           
};







 








