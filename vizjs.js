! function () {
    function t(t) {
        for (; t > o;) t -= o;
        for (; 0 > t;) t += o;
        return t
    }
    function n(t, n, e, r, a, i) {
        if (0 >= a || 0 >= r) return 0;
        var u = t.concat().sort(d3.ascending), o = r - u.length * n +(i ? n: 0), s =[], c = 0, d = 0;
        return d3.range(u.length).forEach(function (t) {
            d =(o - e * t) /(a -= u[t -1] || 0), c += u[t] * d <= e ? 1: 0, s.push(d)
        }), s[c]
    }
    function e(t, e, r, a, i) {
        var u = a, o = d3.sum(t), s = n(t, e, r, i - a, o, ! 1), c = t.map(function (t) {
            var n = s * t, a =(r > n ? r: n) / 2; return u += 2 * a + e, {
                c: u - a, v: n, w: a, value: t, percent: t /(o || 1)
            }
        });
        return c
    }
    function r(t) {
        function n(t, n) {
            return[t * Math.cos(n), t * Math.sin(n)]
        }
        var e = n(t[0], t[2]), r = n(t[0], t[3]), a = n(t[1], t[2]), i = n(t[1], t[3]);
        return[ "M", e, "A", t[0], t[0], "0", t[3] - t[2] > s ? 1: 0, "1", r, "L", i, "A", t[1], t[1], "0", t[3] - t[2] > s ? 1: 0, "0", a, "z"].join(" ")
    }
    function a(t, n, e, r, a, i) {
        function u(t, n, e) {
            return "A" + t + "," + t + " 0 " + +(e > s) + ",1 " + n
        }
        function c(t, n) {
            return[t * Math.cos(n), t * Math.sin(n)]
        }
        function d(t, n, e, r) {
            e += n > e ? o: 0;
            var a = e - n, i = 1 -(a > s ? o - a: a) / s;
            i = Math.pow(i, 5);
            var u =(e + n) / 2 -(e - n > s ? s: 0);
            return "Q" + i * r * Math.cos(u) + "," + i * r * Math.sin(u) + " " + t
        }
        var f = c(t, n), l = c(t, e), g = c(r, a), p = c(r, i);
        return "M" + f + u(t, l, e - n) +(n == a && e == i ? d(f, n, e, r): d(g, e, a, r) + u(r, p, i - a) + d(f, n, i, t)) + "Z"
    }
    function i(t, n) {
        ret =[];
        for (var e = n; e > n - t; e--) ret.push(0 > e ? e + t: e);
        return ret
    }
    var u = {
        version: "1.1.0"
    },
    o = 2 * Math.PI, s = Math.PI, c = Math.PI / 2;
    u.bP = function () {
        function t(i) {
            y = i, i.each(function () {
                var i = d3.select(this), u = t.bars(), o = i.selectAll(".subBars").data(u.subBars).enter().append("g").attr("transform", function (t) {
                    return "translate(" + t.x + "," + t.y + ")"
                }).attr("class", "subBars").append("rect").attr("x", n).attr("y", e).attr("width", r).attr("height", a);
                "undefined" != typeof h && o.style("fill", function (t) {
                    return h(t)
                });
                var s = i.selectAll(".edges").data(u.edges).enter().append("path").attr("class", "edges").attr("d", function (t) {
                    return t.path
                }).style("fill-opacity", t.edgeOpacity());
                "undefined" != typeof h && s.style("fill", function (t) {
                    return h(t)
                }), i.selectAll(".mainBars").data(u.mainBars).enter().append("g").attr("transform", function (t) {
                    return "translate(" + t.x + "," + t.y + ")"
                }).attr("class", "mainBars").append("rect").attr("x", n).attr("y", e).attr("width", r).attr("height", a).style("fill-opacity", 0).on("mouseover", t.mouseover).on("mouseout", t.mouseout)
            })
        }
        function n(t) {
            return - t.width
        }
        function e(t) {
            return - t.height
        }
        function r(t) {
            return 2 * t.width
        }
        function a(t) {
            return 2 * t.height
        }
        var i, u, o, s, c, d, f, l, g, p, h, y, m, v, A, x, k;
        return t.data = function (n) {
            return arguments.length ?(p = n, t): p
        },
        t.fill = function (n) {
            return arguments.length ?(h = n, t): h
        },
        t.keyPrimary = function (n) {
            return arguments.length ?(i = n, t): "undefined" != typeof i ? i: function (t) {
                return t[0]
            }
        },
        t.sortPrimary = function (n) {
            return arguments.length ?(A = n, t): "undefined" != typeof A ? A: d3.ascending
        },
        t.keySecondary = function (n) {
            return arguments.length ?(u = n, t): "undefined" != typeof u ? u: function (t) {
                return t[1]
            }
        },
        t.sortSecondary = function (n) {
            return arguments.length ?(x = n, t): "undefined" != typeof x ? x: d3.ascending
        },
        t.value = function (n) {
            return arguments.length ?(o = n, t): "undefined" != typeof o ? o: function (t) {
                return t[2]
            }
        },
        t.width = function (n) {
            return arguments.length ?(s = n, t): "undefined" != typeof s ? s: 400
        },
        t.height = function (n) {
            return arguments.length ?(c = n, t): "undefined" != typeof c ? c: 600
        },
        t.barSize = function (n) {
            return arguments.length ?(f = n, t): "undefined" != typeof f ? f: 35
        },
        t.min = function (n) {
            return arguments.length ?(l = n, t): "undefined" != typeof l ? l: 0
        },
        t.orient = function (n) {
            return arguments.length ?(d = n, t): "undefined" != typeof d ? d: "vertical"
        },
        t.pad = function (n) {
            return arguments.length ?(g = n, t): "undefined" != typeof g ? g: 0
        },
        t.duration = function (n) {
            return arguments.length ?(v = n, t): "undefined" != typeof v ? v: 500
        },
        t.edgeOpacity = function (n) {
            return arguments.length ?(m = n, t): "undefined" != typeof m ? m: .4
        },
        t.edgeMode = function (n) {
            return arguments.length ?(k = n, t): "undefined" != typeof k ? k: "curved"
        },
        t.bars = function (n) {
            function e(t, e) {
                return "undefined" == typeof n || n.part === e || d[n.part](t) === n.key
            }
            function r() {
                var n = t.min() / 2;
                s.primary.forEach(function (t) {
                    t.height < n &&(t.height = n)
                }), s.secondary.forEach(function (t) {
                    t.height < n &&(t.height = n)
                })
            }
            function a(n) {
                function r(r) {
                    return e(r, n) ? t.value()(r): 0
                }
                var a = d3.nest().key("primary" == n ? t.keyPrimary(): t.keySecondary()).sortKeys("primary" == n ? t.sortPrimary(): t.sortSecondary()).rollup(function (t) {
                    return d3.sum(t, r)
                }).entries(t.data()), i = o(a, t.pad(), t.min(), 0, "vertical" == f ? t.height(): t.width()), u = t.barSize();
                a.forEach(function (e, r) {
                    s[n].push({
                        x: "horizontal" == f ?(i[r].s + i[r].e) / 2: "primary" == n ? u / 2: t.width() - u / 2, y: "vertical" == f ?(i[r].s + i[r].e) / 2: "primary" == n ? u / 2: t.height() - u / 2, height: "vertical" == f ?(i[r].e - i[r].s) / 2: u / 2, width: "horizontal" == f ?(i[r].e - i[r].s) / 2: u / 2, part: n, key: e.key, value: e.value, percent: i[r].p
                    })
                })
            }
            function i(n) {
                function r(r) {
                    return e(r, n) ? t.value()(r): 0
                }
                var a = d3.map(s[n], function (t) {
                    return t.key
                }), i = d3.nest().key("primary" == n ? t.keyPrimary(): t.keySecondary()).sortKeys("primary" == n ? t.sortPrimary(): t.sortSecondary()).key("secondary" == n ? t.keyPrimary(): t.keySecondary()).sortKeys("secondary" == n ? t.sortPrimary(): t.sortSecondary()).rollup(function (t) {
                    return d3.sum(t, r)
                }).entries(t.data());
                i.forEach(function (e) {
                    var r = a. get (e.key), i = o(e.values, 0, 0, "vertical" == f ? r.y - r.height: r.x - r.width, "vertical" == f ? r.y + r.height: r.x + r.width), u = t.barSize();
                    e.values.forEach(function (a, o) {
                        c[n].push({
                            x: "vertical" == f ? "primary" == n ? u / 2: t.width() - u / 2:(i[o].s + i[o].e) / 2, y: "horizontal" == f ? "primary" == n ? u / 2: t.height() - u / 2:(i[o].s + i[o].e) / 2, height:("vertical" == f ? i[o].e - i[o].s: u) / 2, width:("horizontal" == f ? i[o].e - i[o].s: u) / 2, part: n, primary: "primary" == n ? e.key: a.key, secondary: "primary" == n ? a.key: e.key, value: a.value, percent: i[o].p * r.percent, index: "primary" == n ? e.key + "|" + a.key: a.key + "|" + e.key
                        })
                    })
                })
            }
            function u() {
                function n(t, n, e, r, i, u, o, s) {
                    if ("straight" == a) return[ "M", t, ",", n, "L", e, ",", r, "L", i, ",", u, "L", o, ",", s, "z"].join("");
                    var c =(t + e) / 2, d =(i + o) / 2;
                    return[ "M", t, ",", n, "C", c, ",", n, " ", c, ",", r, ",", e, ",", r, "L", i, ",", u, "C", d, ",", u, " ", d, ",", s, ",", o, ",", s, "z"].join("")
                }
                function e(t, n, e, r, i, u, o, s) {
                    if ("straight" == a) return[ "M", t, ",", n, "L", e, ",", r, "L", i, ",", u, "L", o, ",", s, "z"].join("");
                    var c =(n + r) / 2, d =(u + s) / 2;
                    return[ "M", t, ",", n, "C", t, ",", c, " ", e, ",", c, ",", e, ",", r, "L", i, ",", u, "C", i, ",", d, " ", o, ",", d, ",", o, ",", s, "z"].join("")
                }
                var r = d3.map(c.secondary, function (t) {
                    return t.index
                }), a = t.edgeMode();
                return c.primary.map(function (t) {
                    var a = r. get (t.index);
                    return {
                        path: "vertical" === f ? n(t.x + t.width, t.y + t.height, a.x - a.width, a.y + a.height, a.x - a.width, a.y - a.height, t.x + t.width, t.y - t.height): e(t.x - t.width, t.y + t.height, a.x - a.width, a.y - a.height, a.x + a.width, a.y - a.height, t.x + t.width, t.y + t.height), primary: t.primary, secondary: t.secondary, value: t.value, percent: t.percent
                    }
                })
            }
            function o(t, n, e, r, a) {
                var i = e /(a - r -2 * t.length * n), u = 0, o = 0, s = d3.sum(t, function (t) {
                    return t.value
                });
                t.forEach(function (t) {
                    t.value < i * s &&(u += 1, o += t.value)
                });
                var c = 1e-5 > s ? 0:(a - r -2 * t.length * n - u * e) /(s - o), d = r, f =[];
                return t.forEach(function (t) {
                    var r = t.value * c; f.push({
                        s: d + n +(e > r ? .5 *(e - r): 0), e: d + n +(e > r ? .5 *(e + r): r), p: 1e-5 > s ? 0: t.value / s
                    }), d += 2 * n +(e > r ? e: r)
                }), f
            }
            var s = {
                primary:[], secondary:[]
            },
            c = {
                primary:[], secondary:[]
            },
            d = {
                primary: t.keyPrimary(), secondary: t.keySecondary()
            },
            f = t.orient();
            return a("primary"), a("secondary"), i("primary"), i("secondary"), r(), {
                mainBars: s.primary.concat(s.secondary), subBars: c.primary.concat(c.secondary), edges: u()
            }
        },
        t.mouseover = function (i) {
            var u = t.bars(i);
            y.selectAll(".mainBars").filter(function (t) {
                return t.part === i.part && t.key === i.key
            }).select("rect").style("stroke-opacity", 1), y.selectAll(".subBars").data(u.subBars).transition().duration(t.duration()).attr("transform", function (t) {
                return "translate(" + t.x + "," + t.y + ")"
            }).select("rect").attr("x", n).attr("y", e).attr("width", r).attr("height", a);
            var o = y.selectAll(".edges").data(u.edges);
            o.filter(function (t) {
                return t[i.part] === i.key
            }).transition().duration(t.duration()).style("fill-opacity", t.edgeOpacity()).attr("d", function (t) {
                return t.path
            }), o.filter(function (t) {
                return t[i.part] !== i.key
            }).transition().duration(t.duration()).style("fill-opacity", 0).attr("d", function (t) {
                return t.path
            }), y.selectAll(".mainBars").data(u.mainBars).transition().duration(t.duration()).attr("transform", function (t) {
                return "translate(" + t.x + "," + t.y + ")"
            }).select("rect").attr("x", n).attr("y", e).attr("width", r).attr("height", a)
        },
        t.mouseout = function (i) {
            var u = t.bars();
            y.selectAll(".mainBars").filter(function (t) {
                return t.part === i.part && t.key === i.key
            }).select("rect").style("stroke-opacity", 0), y.selectAll(".subBars").data(u.subBars).transition().duration(t.duration()).attr("transform", function (t) {
                return "translate(" + t.x + "," + t.y + ")"
            }).select("rect").attr("x", n).attr("y", e).attr("width", r).attr("height", a), y.selectAll(".edges").data(u.edges).transition().duration(t.duration()).style("fill-opacity", t.edgeOpacity()).attr("d", function (t) {
                return t.path
            }), y.selectAll(".mainBars").data(u.mainBars).transition().duration(t.duration()).attr("transform", function (t) {
                return "translate(" + t.x + "," + t.y + ")"
            }).select("rect").attr("x", n).attr("y", e).attr("width", r).attr("height", a)
        },
        t
    },
    u.gg = function () {
        function t(n) {
            g = n, n.each(function () {
                var n = d3.select(this), e = t.scale(), r = t.minorTickStart(), a = t.minorTickEnd(), i = t.majorTickStart(), u = t.majorTickEnd(), o = t.ticks(), c = t.majorTicks(), d = t.labelLocation(), f = t.outerRadius();
                n.append("circle").attr("r", f).style("fill", "url(#vizgg3" + p + ")").attr("class", "face"), n.append("circle").attr("r", t.innerRadius()).style("fill", "url(#vizgg2" + p + ")").style("filter", "url(#vizgg5" + p + ")").attr("class", "innerFace");
                var l = n.append("g");
                l.selectAll("line").data(o).enter().append("line").attr("class", function (t) {
                    return c(t) ? "majorTicks": "minorTicks"
                }).attr("x1", function (t) {
                    return f *(c(t) ? i: r) * Math.cos(e(t))
                }).attr("y1", function (t) {
                    return f *(c(t) ? i: r) * Math.sin(e(t))
                }).attr("x2", function (t) {
                    return f *(c(t) ? u: a) * Math.cos(e(t))
                }).attr("y2", function (t) {
                    return f *(c(t) ? u: a) * Math.sin(e(t))
                }), n.selectAll("text").data(o.filter(c)).enter().append("text").attr("class", "label").attr("x", function (t) {
                    return f * d * Math.cos(e(t))
                }).attr("y", function (t) {
                    return f * d * Math.sin(e(t))
                }).attr("dy", 3).text(function (t) {
                    return t
                });
                var g = t.outerRadius() / b.outerRadius, h = 180 * t.scale()(t.value()) / s + 90; n.append("g").attr("transform", "translate(1,1)").selectAll(".needleshadow").data([0]).enter().append("g").attr("transform", "rotate(" + h + ")").attr("class", "needleshadow").append("path").attr("d",[ "m 0", -130 * g, 5 * g, 175 * g, -10 * g, "0,z"].join(",")).style("filter", "url(#vizgg6" + p + ")"), n.selectAll(".needle").data([0]).enter().append("g").attr("transform", "rotate(" + h + ")").attr("class", "needle").append("polygon").attr("points",[-.5 * g, -130 * g, .5 * g, -130 * g, 5 * g, 45 * g, -5 * g, 45 * g].join(",")).style("fill", "url(#vizgg4" + p + ")")
            })
        }
        var n, e, r, a, i, o, c, d, f, l, g, p, h, y, m, v, A, x, k, b = {
            innerRadius: 20, outerRadius: 150, angleOffset: .7, startAngle: -1.5 * s, endAngle: .5 * s, minorTickStart: .9, minorTickEnd: .95, majorTickStart: .82, majorTickEnd: .95, needleColor: "#de2c2c", innerFaceColor: "#999999", faceColor: "#666666", domain:[0, 100], duration: 500, ease: "cubicInOut", ticks: d3.range(0, 101, 2), majorTicks: function (t) {
                return t % 10 === 0
            },
            labelLocation: .7
        };
        return t.scale = function () {
            return d3.scale.linear().domain(t.domain()).range([b.startAngle + t.angleOffset(), b.endAngle - t.angleOffset()])
        },
        t.innerRadius = function (e) {
            return arguments.length ?(n = e, t): "undefined" != typeof n ? n: b.innerRadius
        },
        t.outerRadius = function (n) {
            return arguments.length ?(e = n, t): "undefined" != typeof e ? e: b.outerRadius
        },
        t.angleOffset = function (n) {
            return arguments.length ?(d = n, t): "undefined" != typeof d ? d: b.angleOffset
        },
        t.labelLocation = function (n) {
            return arguments.length ?(k = n, t): "undefined" != typeof k ? k: b.labelLocation
        },
        t.ticks = function (n) {
            return arguments.length ?(h = n, t): "undefined" != typeof h ? h: b.ticks
        },
        t.majorTicks = function (n) {
            return arguments.length ?(y = n, t): "undefined" != typeof y ? y: b.majorTicks
        },
        t.minorTickStart = function (n) {
            return arguments.length ?(m = n, t): "undefined" != typeof m ? m: b.minorTickStart
        },
        t.minorTickEnd = function (n) {
            return arguments.length ?(v = n, t): "undefined" != typeof v ? v: b.minorTickEnd
        },
        t.majorTickStart = function (n) {
            return arguments.length ?(A = n, t): "undefined" != typeof A ? A: b.majorTickStart
        },
        t.majorTickEnd = function (n) {
            return arguments.length ?(x = n, t): "undefined" != typeof x ? x: b.majorTickEnd
        },
        t.needleColor = function (n) {
            return arguments.length ?(r = n, t): "undefined" != typeof r ? r: b.needleColor
        },
        t.innerFaceColor = function (n) {
            return arguments.length ?(a = n, t): "undefined" != typeof a ? a: b.innerFaceColor
        },
        t.faceColor = function (n) {
            return arguments.length ?(i = n, t): "undefined" != typeof i ? i: b.faceColor
        },
        t.domain = function (n) {
            return arguments.length ?(o = n, t): "undefined" != typeof o ? o: b.domain
        },
        t.duration = function (n) {
            return arguments.length ?(f = n, t): "undefined" != typeof f ? f: b.duration
        },
        t.ease = function (n) {
            return arguments.length ?(l = n, t): "undefined" != typeof l ? l: b.ease
        },
        t.value = function (n) {
            return arguments.length ?(c = n, t): "undefined" != typeof c ? c: .5 *(b.domain[0] + b.domain[1])
        },
        t.defs = function (n, e) {
            var r = n.append("defs");
            p = e;
            var a = t.needleColor(), i = t.innerFaceColor(), o = t.faceColor(), s = u.defs(r).lG().id("vizgg1" + e).sel();
            u.defs(s).stop().offset("0").stopColor(a), u.defs(s).stop().offset("1").stopColor(d3.rgb(a).darker(1));
            var c = u.defs(r).rG().id("vizgg2" + e).fx("35%").fy("65%").r("65%").spreadMethod("pad").sel();
            u.defs(c).stop().offset("0").stopColor(i), u.defs(c).stop().offset("1").stopColor(d3.rgb(i).darker(2));
            var d = u.defs(r).rG().id("vizgg3" + e).fx("35%").fy("65%").r("65%").spreadMethod("pad").sel();
            u.defs(d).stop().offset("0").stopColor(o), u.defs(d).stop().offset("1").stopColor(d3.rgb(o).darker(2)), u.defs(r).lG().id("vizgg4" + e).gradientUnits("userSpaceOnUse").y1("80").x1("-10").y2("80").x2("10").xlink("#vizgg1" + e);
            var f = u.defs(r).filter().id("vizgg5" + e).sel();
            u.defs(f).feFlood().result("flood").floodColor("rgb(0,0,0)").floodOpacity("0.6"), u.defs(f).feComposite().result("composite1").operator("in").in2("SourceGraphic")[ "in"]("flood"), u.defs(f).feGaussianBlur().result("blur").stdDeviation("2")[ "in"]("composite1"), u.defs(f).feOffset().result("offset").dy("2").dx("2"), u.defs(f).feComposite().result("composite2").operator("over").in2("offset")[ "in"]("SourceGraphic");
            var l = u.defs(r).filter().x("-0.3").y("-0.3").height("1.8").width("1.8").id("vizgg6" + e).sel();
            u.defs(l).feGaussianBlur().stdDeviation("2")
        },
        t.setNeedle = function (n) {
            function e(t, n) {
                return d3.interpolateString("rotate(" + t + ")", "rotate(" + n + ")")
            }
            var r = 180 * t.scale()(n) / s + 90, a = 180 * t.scale()(t.value()) / s + 90, i = t.ease();
            g.selectAll(".needle").data([n]).transition().duration(t.duration()).attrTween("transform", function (t) {
                return e(a, r)
            }).ease(i), g.selectAll(".needleshadow").data([n]).transition().duration(t.duration()).attrTween("transform", function (t) {
                return e(a, r)
            }).ease(i).each("end", function () {
                angle = n
            }), t.value(n)
        },
        t
    },
    u.ch = function () {
        function u(t) {
            function n(t) {
                return r([B, O, t.startAngle, t.endAngle])
            }
            function e(t) {
                return a(B, t.startAngle, t.endAngle, B, t.endStartAngle, t.endEndAngle)
            }
            p = t, k || f(), t.each(function () {
                var t = d3.select(this), r = t.selectAll(".groups").data(x).enter().append("g").attr("class", "groups").on("mouseover", u.mouseover).on("mouseout", u.mouseout), a = r.append("text").attr("class", "label"), i =(1 + u.labelPadding()) * u.outerRadius(), s = u.valueFormat();
                a.filter(function (t) {
                    return "g" == t.type
                }).attr("x", function (t) {
                    return i * Math.cos(d(t))
                }).attr("y", function (t) {
                    return i * Math.sin(d(t))
                }).text(function (t) {
                    return t.source + " (" + s(t.value) + ")"
                }).style("text-anchor", function (t) {
                    var n = d(t);
                    return c > n || n > o - c ? "start": "end"
                }).each(function (t) {
                    this._current = t
                }), r.append("path").style("fill", function (t) {
                    return y(t.source)
                }).style("stroke", function (t) {
                    return y(t.source)
                }).attr("d", n).each(function (t) {
                    this._current = t
                }).filter(function (t) {
                    return "g" == t.type
                }), t.append("g").attr("class", "chords").selectAll(".chord").data(k).enter().append("g").attr("class", "chord").append("path").each(function (t) {
                    this._current = t
                }).attr("d", e).style("fill", function (t) {
                    return y(t.target)
                }).style("opacity", u.chordOpacity()).style("stroke", function (t) {
                    return y(t.target)
                }).style("display", function (t) {
                    return t.display ? "inline": "none"
                })
            })
        }
        function s(t) {
            function n(t, n) {
                return r([B, O, t.startAngle, t.endAngle])
            }
            function e(t) {
                return a(B, t.startAngle, t.endAngle, B, t.endStartAngle, t.endEndAngle)
            }
            function i(t) {
                var n = d3.interpolate(this._current, t);
                return this._current = n(0), function (t) {
                    return e(n(t))
                }
            }
            function s(t) {
                var e = d3.interpolate(this._current, t);
                return this._current = e(0), function (t) {
                    return n(e(t), t)
                }
            }
            function f(t) {
                var n = d3.interpolate(this._current, t);
                return this._current = n(0), function (t) {
                    return h * Math.cos(d(n(t)))
                }
            }
            function l(t) {
                var n = d3.interpolate(this._current, t);
                return this._current = n(0), function (t) {
                    return h * Math.sin(d(n(t)))
                }
            }
            var g = p.selectAll(".groups").data(t ? b: u.groups()), h =(1 + u.labelPadding()) * u.outerRadius(), y = u.valueFormat();
            g.select("path").transition().duration(L).attrTween("d", s), g.select(".label").filter(function (t) {
                return "g" == t.type
            }).transition().duration(L).attrTween("x", f).attrTween("y", l).text(function (t) {
                return t.source + " (" + y(t.value) + ")"
            }).style("text-anchor", function (t) {
                var n = d(t);
                return c > n || n > o - c ? "start": "end"
            });
            var m = u.chordOpacity();
            p.select(".chords").selectAll(".chord").data(t ? w: u.chords()).select("path").transition().duration(L).attrTween("d", i).style("opacity", function (t) {
                return t.display ? m: 0
            })
        }
        function d(n) {
            return t((n.startAngle + n.endAngle) / 2)
        }
        function f() {
            m =[], h.forEach(function (t) { -1 == m.indexOf(E(t)) && m.push(E(t)), -1 == m.indexOf(M(t)) && m.push(M(t))
            }), m = m.sort(T), v = {
            },
            A = {
            },
            m.forEach(function (t) {
                v[t] = {
                },
                A[t] = {
                },
                m.forEach(function (n) {
                    v[t][n] = 0, A[t][n] = ! 1
                })
            }), h.forEach(function (t) {
                var n = E(t), e = M(t);
                v[n][e] += S(t), A[n][e] = ! 0
            }), x =[], m.forEach(function (t, n) {
                x.push({
                    source: t, type: "gs", value: 0, skipPad: ! 0, index: n
                }), x.push({
                    source: t, type: "g", value: d3.sum(m, function (n) {
                        return v[t][n]
                    }), skipPad: ! 1, index: n
                })
            }), g(x, z, R, void 0, P), k =[], x.filter(function (t) {
                return "g" == t.type
            }).forEach(function (t, n) {
                var r = i(m.length, n), a = e(r.map(function (n) {
                    return v[t.source][m[n]]
                }), 0, 0, t.startAngle, t.endAngle);
                r.forEach(function (e, r) {
                    var i = a[r]; k.push({
                        startAngle: i.c - i.v / 2, endAngle: i.c + i.v / 2, value: i.value, source: t.source, target: m[e], type: "c", display: A[t.source][m[e]], index: n, subindex: e, indexsubindex: n + "-" + e
                    })
                })
            });
            var t = d3.map(k, function (t) {
                return t.indexsubindex
            });
            k.forEach(function (n) {
                if (n.subindex == n.index) return n.endStartAngle = n.startAngle, void (n.endEndAngle = n.startAngle);
                var e = t. get (n.subindex + "-" + n.index);
                n.endStartAngle = e.startAngle, n.endEndAngle = e.startAngle
            }), F = ! 1
        }
        function l(t) {
            function n(t) {
                return t.endAngle + t.startAngle
            }
            var r = x.filter(function (n) {
                return n.source == t && "g" == n.type
            })[0];
            b =[], m.forEach(function (n, e) {
                b.push({
                    source: n, startAngle: r.startAngle, endAngle: r.endAngle, padAngle: r.padAngle, percent: r.percent, type: "gs", value: n == t ? v[n][n]: 0, skipPad: n == t && A[n][n] ? ! 1: ! 0, index: e
                }), n == t ? b.push({
                    source: n, startAngle: r.startAngle, endAngle: r.endAngle, padAngle: r.padAngle, percent: r.percent, type: "g", value: r.value, skipPad: ! 1, index: e
                }): b.push({
                    source: n, type: "g", value: v[t][n], skipPad: ! 1, index: e
                })
            }), g(b, z, R, t, P);
            var a = n(r);
            x.forEach(function (t, e) {
                var r = b[e], i = n(t) < a; r.startAngle -= i ? o: 0, r.endAngle -= i ? o: 0
            }), w =[], b.filter(function (t) {
                return "g" == t.type
            }).forEach(function (n, r) {
                var a = i(m.length, r), u = a.map(function (e) {
                    var r = m[e]; return n.source == t ? v[n.source][r]: r == t ? v[r][n.source]: 0
                }), o = e(u, 0, 0, n.startAngle, n.endAngle);
                a.forEach(function (e, a) {
                    var i = o[a]; w.push({
                        startAngle: i.c - i.v / 2, endAngle: i.c + i.v / 2, value: i.value, source: n.source, target: m[e], type: "c", display: n.source === t, index: r, subindex: e, indexsubindex: r + "-" + e
                    })
                })
            });
            var u = d3.map(w.map(function (t) {
                return {
                    startAngle: t.startAngle, endAngle: t.endAngle, indexsubindex: t.indexsubindex
                }
            }), function (t) {
                return t.indexsubindex
            }), s = d3.map(b.filter(function (t) {
                return "gs" == t.type
            }), function (t) {
                return t.source
            });
            w.forEach(function (n) {
                if (n.subindex == n.index) {
                    var e = s. get (n.source);
                    return n.endStartAngle = e.startAngle, void (n.endEndAngle = e.endAngle)
                }
                var r = u. get (n.subindex + "-" + n.index);
                n.endStartAngle = r.startAngle, n.endEndAngle = r.endAngle, n.source !== t &&(n.startAngle = n.endAngle, n.endEndAngle = n.endStartAngle)
            })
        }
        function g(t, e, r, a, i) {
            var u = void 0 !== a, o = 0;
            if (u) {
                for (var s = ! 1; o < t.length; o++) if (t[o].source == a && "g" == t[o].type) {
                    s = ! 0;
                    break
                }
                s || console.log("The fixed source '" + a + "' is not a valid key")
            }
            var c = d3.range(t.length);
            u &&(c = c.slice(o).concat(c.slice(0, o)));
            var d = t.filter(function (t) {
                return !(u && t.source === a && "g" == t.type || t.skipPad)
            }).map(function (t) {
                return t.value
            }), f = 2 * Math.PI -(u ? t[o].endAngle - t[o].startAngle + 2 * e: 0), l = u ? t[o].endAngle + e: i, g = d3.sum(d), p = n(d, e, r, 0 >= f ? 0: f, g, u ? ! 0: ! 1);
            c.slice(u ? 1: 0).forEach(function (n) {
                var a = p * t[n].value, i =(r > a ? r - a: 0) / 2; t[n].startAngle = l, t[n].endAngle = l + a, t[n].padAngle = i, t[n].percent = t[n].value /(g || 1), l += a + i +(t[n].skipPad ? 0: e)
            })
        }
        var p, h, y, m, v, A, x, k, b, w, E = function (t) {
            return t[0]
        },
        M = function (t) {
            return t[1]
        },
        S = function (t) {
            return t[2]
        },
        C = function (t) {
            return t
        },
        T = d3.ascending, z = .03, P = 0, B = 180, O = 200, j = .7, G = .02, R = 0, L = 500, F = ! 0;
        return u.data = function (t) {
            return arguments.length ?(h = t, F = ! 0, u): h
        },
        u.fill = function (t) {
            return arguments.length ?(y = t, u): y
        },
        u.duration = function (t) {
            return arguments.length ?(L = t, u): L
        },
        u.chordOpacity = function (t) {
            return arguments.length ?(j = t, u): j
        },
        u.innerRadius = function (t) {
            return arguments.length ?(B = t, F = ! 0, u): B
        },
        u.outerRadius = function (t) {
            return arguments.length ?(O = t, F = ! 0, u): O
        },
        u.source = function (t) {
            return arguments.length ?(E = t, F = ! 0, u): E
        },
        u.target = function (t) {
            return arguments.length ?(M = t, F = ! 0, u): M
        },
        u.value = function (t) {
            return arguments.length ?(S = t, F = ! 0, u): S
        },
        u.padding = function (t) {
            return arguments.length ?(z = t, F = ! 0, u): z
        },
        u.labelPadding = function (t) {
            return arguments.length ?(G = t, u): G
        },
        u.sort = function (t) {
            return arguments.length ?(T = t, F = ! 0, u): T
        },
        u.startAngle = function (t) {
            return arguments.length ?(P = t, F = ! 0, u): P
        },
        u.chords = function () {
            return F && f(), k
        },
        u.groups = function () {
            return F && f(), x
        },
        u.valueFormat = function (t) {
            return arguments.length ?(C = t, u): C
        },
        u.mouseover = function (t) {
            l(t.source), s(1)
        },
        u.mouseout = function (t) {
            s(0)
        },
        u
    },
    u.defs = function (t) {
        var n = {
        },
        e = t;
        return n.sel = function () {
            return e
        },
        n.lG = function () {
            return e = e.append("linearGradient"), n
        },
        n.rG = function () {
            return e = e.append("radialGradient"), n
        },
        n.stop = function () {
            return e = e.append("stop"), n
        },
        n.filter = function () {
            return e = e.append("filter"), n
        },
        n.feFlood = function () {
            return e = e.append("feFlood"), n
        },
        n.feComposite = function () {
            return e = e.append("feComposite"), n
        },
        n.feOffset = function () {
            return e = e.append("feOffset"), n
        },
        n.feGaussianBlur = function () {
            return e = e.append("feGaussianBlur"), n
        },
        n.result = function (t) {
            return e = e.attr("result", t), n
        },
        n.floodColor = function (t) {
            return e = e.attr("flood-color", t), n
        },
        n.floodOpacity = function (t) {
            return e = e.attr("flood-opacity", t), n
        },
        n.stdDeviation = function (t) {
            return e = e.attr("stdDeviation", t), n
        },
        n.operator = function (t) {
            return e = e.attr("operator", t), n
        },
        n.height = function (t) {
            return e = e.attr("height", t), n
        },
        n.width = function (t) {
            return e = e.attr("width", t), n
        },
        n[ "in"] = function (t) {
            return e = e.attr("in", t), n
        },
        n.in2 = function (t) {
            return e = e.attr("in2", t), n
        },
        n.id = function (t) {
            return e = e.attr("id", t), n
        },
        n.fx = function (t) {
            return e = e.attr("fx", t), n
        },
        n.fy = function (t) {
            return e = e.attr("fy", t), n
        },
        n.dx = function (t) {
            return e = e.attr("dx", t), n
        },
        n.dy = function (t) {
            return e = e.attr("dy", t), n
        },
        n.x1 = function (t) {
            return e = e.attr("x1", t), n
        },
        n.y1 = function (t) {
            return e = e.attr("y1", t), n
        },
        n.x2 = function (t) {
            return e = e.attr("x2", t), n
        },
        n.y2 = function (t) {
            return e = e.attr("y2", t), n
        },
        n.x = function (t) {
            return e = e.attr("x", t), n
        },
        n.y = function (t) {
            return e = e.attr("y", t), n
        },
        n.r = function (t) {
            return e = e.attr("r", t), n
        },
        n.spreadMethod = function (t) {
            return e = e.attr("spreadMethod", t), n
        },
        n.gradientUnits = function (t) {
            return e = e.attr("gradientUnits", t), n
        },
        n.xlink = function (t) {
            return e = e.attr("xlink:href", t), n
        },
        n.offset = function (t) {
            return e = e.attr("offset", t), n
        },
        n.stopColor = function (t) {
            return e = e.attr("stop-color", t), n
        },
        n.path = function () {
            return e = e.append("path"), n
        },
        n.d = function (t) {
            return e = e.attr("d", t), n
        },
        n
    },
    this.viz = u
}
();