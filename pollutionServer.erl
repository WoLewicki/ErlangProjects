%%%-------------------------------------------------------------------
%%% @author wojlewy
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. maj 2018 19:11
%%%-------------------------------------------------------------------
-module(pollutionServer).
-author("wojlewy").

%% API
-import(pollution,[createMonitor/0, addStation/3, addValue/5, removeValue/4, getOneValue/4, getStationMean/3, getDailyMean/3, getHourlyMean/4]).
-export([start/0, stop/0, loop/1]).
-export([addStation/2, addValue/4, removeValue/3, getOneValue/3, getStationMean/2, getDailyMean/2, getHourlyMean/3]).


%%server

start() ->
  register(pollutionserver, spawn(?MODULE, loop, [pollution:createMonitor()])).

stop() ->
  pollutionserver ! stop.

loop(Monitor) ->
  receive
    stop -> ok;
    {request, Pid, {addStation, List}} ->
      Result = pollution:addStation(lists:nth(1, List), lists:nth(2, List), Monitor),
      case Result of
        {error, String} -> Pid ! {reply, String}, loop(Monitor);
        P -> Pid ! {reply, ok}, loop(P)
      end;
    {request, Pid, {addValue, List}} ->
      Result = pollution:addValue(lists:nth(1, List), lists:nth(2, List), lists:nth(3, List), lists:nth(4, List), Monitor),
      case Result of
        {error, String} -> Pid ! {reply, String}, loop(Monitor);
        P -> Pid ! {reply, ok}, loop(P)
      end;
    {request, Pid, {removeValue, List}} ->
      Result = pollution:removeValue(lists:nth(1, List), lists:nth(2, List), lists:nth(3, List), Monitor),
      case Result of
        {error, String} -> Pid ! {reply, String}, loop(Monitor);
        P -> Pid ! {reply, ok}, loop(P)
      end;
    {request, Pid, {getOneValue, List}} ->
      Result = pollution:getOneValue(lists:nth(1, List), lists:nth(2, List), lists:nth(3, List), Monitor),
      case Result of
        {error, String} -> Pid ! {reply, String}, loop(Monitor);
        P -> Pid ! {reply, ok}, loop(P)
      end;
    {request, Pid, {getStationMean, List}} ->
      Result = pollution:getStationMean(lists:nth(1, List), lists:nth(2, List), Monitor),
      case Result of
        {error, String} -> Pid ! {reply, String}, loop(Monitor);
        P -> Pid ! {reply, ok}, loop(P)
      end;
    {request, Pid, {getDailyMean, List}} ->
      Result = pollution:getDailyMean(lists:nth(1, List), lists:nth(2, List), Monitor),
      case Result of
        {error, String} -> Pid ! {reply, String}, loop(Monitor);
        P -> Pid ! {reply, ok}, loop(P)
      end;
    {request, Pid, {getHourlyMean, List}} ->
      Result = pollution:getHourlyMean(lists:nth(1, List), lists:nth(2, List), lists:nth(3, List), Monitor),
      case Result of
        {error, String} -> Pid ! {reply, String}, loop(Monitor);
        P -> Pid ! {reply, ok}, loop(P)
      end
  end.

%%client

call(Message) ->
  pollutionServer ! {request, self(), Message},
  receive
    {reply, Reply} -> Reply
  end.

addStation(String, Cords) -> call({addStation, [String, Cords]}).
addValue(NameOrCords, Date, Type, Value) -> call({addValue, [NameOrCords, Date, Type, Value]}).
removeValue(Station, Date, Type) -> call({removeValue, [Station, Date, Type]}).
getOneValue(Type, Date, Station) -> call({getOneValue, [Type, Date, Station]}).
getStationMean(Type, Station) -> call({getStationMean, [Type, Station]}).
getDailyMean(Type, Day) -> call({getDailyMean, [Type, Day]}).
getHourlyMean(Hour, Type, Station) -> call({getHourlyMean, [Hour, Type, Station]}).