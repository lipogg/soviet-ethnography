
 function sort(sortOrder){
                    return function(a,b){ return d3.ascending(sortOrder.indexOf(a),sortOrder.indexOf(b)) }
                  }

svg.selectAll('*').remove();
var color = {'Unlinked':'#3366CC','':'#FFFFD9','':'#FCFDD2','':'#F9FCCB','':'#F6FBC5','':'#F3FABE','':'#F0F9B8','':'#EDF8B1','':'#E7F5B1','':'#E1F3B1','':'#DBF0B2','':'#D4EEB2','':'#CEECB3','':'#C8E9B3','':'#BEE5B4','':'#B2E1B6','':'#A6DCB7','':'#9AD7B8','':'#8FD3B9','':'#83CEBA','':'#78CABB','':'#6EC6BD','':'#64C3BE','':'#5ABFC0','':'#50BBC1','':'#46B7C3','':'#3EB2C3','':'#38ACC3','':'#32A6C2','':'#2CA0C1','':'#269AC1','':'#2094C0','':'#1D8DBE','':'#1E85BA','':'#1E7DB6','':'#1F74B2','':'#206CAE','':'#2164AA','':'#225CA7','':'#2255A3','':'#234EA0','':'#23479D','':'#24409A','':'#243A96','':'#243392','':'#1F2F88','':'#1A2C7F','':'#162875','':'#11246B','':'#0C2061','':'#081D58'};




var g1 = svg.append("g").attr("transform","translate(625,50)");
                         var bp1=viz.bP()
                         .data(data)
                         .value(d=>d[2])
                         .min(1)
                         .pad(4)
                         .height(1000)
                         .width(300)
                         .barSize(35)
                         .fill(d=>color[d.primary])
.orient("vertical");

g1.call(bp1);g1.append("text")
                        .attr("x",-50).attr("y",-8)
                        .style("text-anchor","middle")
                        .text("Group or Location");
                        g1.append("text")
                        .attr("x", 250)
                        .attr("y",-8).style("text-anchor","middle")
                        .text("Category");
                        g1.append("text")
                        .attr("x",100).attr("y",-25)
                        .style("text-anchor","middle")
                        .attr("class","header")
                        .text("Co-Occurrences");

 g1.selectAll(".mainBars")
                        .on("mouseover",mouseover)
                        .on("mouseout",mouseout);

 g1.selectAll(".mainBars").append("text").attr("class","label")
                        .attr("x",d=>(d.part=="primary"? -100:100))
                        .attr("y",d=>+6)
                        .text(d=>d.key)
                        .attr("text-anchor",d=>(d.part=="primary"? "end": "start"));

 g1.selectAll(".mainBars").append("text").attr("class","perc")
                        .attr("x",d=>(d.part=="primary"? -505:520))
                        .attr("y",d=>+6)
                        .text(function(d){ return d3.format("0.1%")(d.percent)})
                        .attr("text-anchor",d=>(d.part=="primary"? "end": "start")); 

function mouseover(d){
bp1.mouseover(d);
                            g1.selectAll(".mainBars")
                            .select(".perc")
                            .text(function(d){ return d3.format("0.1%")(d.percent)});
}

                     function mouseout(d){
bp1.mouseout(d);
                            g1.selectAll(".mainBars")
                            .select(".perc")
                            .text(function(d){ return d3.format("0.1%")(d.percent)});
}


