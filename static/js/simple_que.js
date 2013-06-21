var LDAP="";
var HOST = "http://localhost";
//TODO promin autherization
function login(){
      var login = document.getElementById("login").value;
      LDAP = login;
      $("#login").hide("fast");
      $("#menu").show("fast");
      $("#queue").show("fast");
}

function fraud_next(){
      var params = "[{\"name\":\"X1\"}, {\"name\":\"X2\"},{\"name\":\"X3\"}, {\"name\":\"X4\"}, {\"name\":\"X5\"},{\"name\":\"X6\"},{\"name\":\"X7\"}, {\"name\":\"X8\"},{\"name\":\"X9\"}, {\"name\":\"X10\"}, {\"name\":\"X11\"},{\"name\":\"X12\"},{\"name\":\"X13\"},{\"name\":\"X14\"}  ]";
      var callback = function(Resp){
	
	    if(Resp["status"] == "ok" ){
		 get_record(Resp["session"]);
	    }else{
		  alert(Resp["description"]);
	      
	    }
	    
      }
      get_session_queue("get_fraud", params, callback );
}

function get_record(Session){
		var new_function = function(Resp){
		if(Resp["status"] == "wait"){
		     if(confirm("Запрос в работе, повторить запрос ?") ){
			  get_record(Session)
		      }else{
			    finish_aim(Session)
		      }
		  
		}else if(Resp["status"]&& Resp["status"]!="wait"){
		      
		      alert(Resp["description"])
		      
		}else{
		      
			draw_record(Resp);
			if(confirm("Еще  ?") ){
			      next_session(Session);
			  
			  
			}else{
			    finish_aim(Session)
			}
		  
		}
	  
	  
	  
	      }  
	
  
	$.ajax({
                        type: "GET",
                        url: HOST+"/prolog/process/"+Session ,
                        data: {},
                        success: new_function
              }
        );
  
  
  
}
function next_session(Session ){
				var new_function = function(){
				    get_record(Session)
				};
				$.ajax({
				  type: "GET",
				  url: HOST+"/prolog/next/"+Session ,
				  data: {},
				  success: new_function
				}
				);
  
  
}

function  finish_aim(Session){
  
      var new_function = function(){};
      $.ajax({
                        type: "GET",
                        url: HOST+"/prolog/finish/"+Session ,
                        data: {},
                        success: new_function
              }
        );
  
  
}

function draw_record(Resp){
  
	  Str ="<div>";
	  Str += "  <div class=\"row-fluid show-grid\">";
	  Str += " <div class=\"span2\" >" + Resp["X1"] + "</div>";
          Str += " <div class=\"span2\">" + Resp["X2"] +" "+Resp["X3"] + "</div>";
          Str += " <div class=\"span2\">" + Resp["X4"] + "</div>";
          Str += " <div class=\"span2\">" + Resp["X5"] + "</div>";
          Str += " <div class=\"span2\">" + Resp["X6"] + "</div>";
 	  Str += " </div> <div class=\"row-fluid show-grid\">";

          Str += " <div class=\"span2\">" +Resp["X11"] +" " + Resp["X7"] + "</div>";
          Str += " <div class=\"span2\">" +Resp["X12"] +" " + Resp["X8"] + "</div>";
	  Str +="  </div>";
	   
	  Str += "<div class=\"row-fluid show-grid\">";
	  Str +=" <div class=\"span4\">"+Resp["X9"]+"</div>";
          Str +=" <div class=\"span4\">"+Resp["X10"]+"</div>";
          Str +=" <div class=\"span4\">"+Resp["X14"]+"</div>";
          Str +="  </div>";
	  
	  Str += "<div class=\"row-fluid show-grid\">";
	  Str +=" <div class=\"span8\">"+Resp["X13"]+"</div>";
          Str +="  </div>";
          Str +=" <a class=\"btn btn-success pull-left\" href=\"javascript:fraud_no('"+ Resp["X1"]+"')\">Нормально</a>\
          <a class=\"btn btn-danger pull-right\" href=\"javascript:fraud_yes('"+ Resp["X1"]+"')\">Мошенник</a>\
          <br/><br/>\
          \
          </div>";  
	  $("#queue").append(Str);
  
}

function get_session_queue(Table, Str, Check){
  
	params = {"params": Str};
        $.ajax({
                        type: "POST",
                        url: HOST+"/prolog/create/"+Table ,
                        data: params,
                        success: Check
                        
                        }
        );
  
}
function fraud_yes(Ref){
	 var Check = function(){};
	 var params = "[\""+Ref+"\",\""+LDAP+"\" ]";
	 $.ajax({
                        type: "POST",
                        url: HOST+"/prolog/create/confirm_fraud_oper",
                        data: params,
                        success: Check
                        }
        );
	
  
  
}
function fraud_no(){
  
}

