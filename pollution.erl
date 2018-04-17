%%%-------------------------------------------------------------------
%%% @author wojlewy
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. kwi 2018 23:46
%%%-------------------------------------------------------------------
-module(pollution).
-author("wojlewy").

%% API
-export([findStation/2, findSameMeasure/3, getMeasure/3, getDay/1, getSumOfList/3, getHour/1, createMonitor/0,
  addStation/3, addValue/5, removeValue/4, getOneValue/4, getStationMean/3, getDailyMean/3, getHourlyMean/4]).

%% local functions
findStation(Station, Monitor) -> lists:filter(fun(X) -> (dict:fetch(name, X) == Station) or (dict:fetch(cords, X) == Station) end, Monitor).
findSameMeasure(Station, Date, Type) -> lists:filter(fun(X) -> (dict:fetch(date, X) == Date) andalso (dict:fetch(type, X) == Type) end, dict:fetch(measurements, Station)).
getMeasure(Station, Date, Type) -> lists:filter(fun(X) -> (dict:fetch(date, X) == Date)  andalso (dict:fetch(type,X) == Type) end, dict:fetch(measurements, Station)).
getDay({D,_}) -> D.
getSumOfList(List, Type, Day) -> lists:sum([dict:fetch(value, X) || X <- List,
  Type == dict:fetch(type, dict:fetch(measurements, X)), Day ==  getDay(dict:fetch(date, dict:fetch(measurements, X)))]).
getHour({_,{H,_,_}}) -> H.

%% needed functions
createMonitor() -> [].
addStation(String, Cords, Monitor) -> case findStation(String, Monitor) of
                                        []-> [dict:from_list([{name, String},{cords, Cords}]) | Monitor];
                                        _ -> {error, "There is already this station."}
                                      end.
addValue(NameOrCords, Date, Type, Value, Monitor) ->
  case findStation(NameOrCords, Monitor) of
    [] -> {error, "There is no such station."};
    [X|[]] -> case dict:is_key(measurements, X) of
                false -> [dict:store(measurements, [dict:from_list([{date, Date},{type,Type},{value,Value}])], X) | lists:delete(X, Monitor)];
                true ->
      case findSameMeasure(X, Date, Type) of
                [] -> [dict:store(measurements, [dict:from_list([{date, Date},{type,Type},{value,Value}])], X) | lists:delete(X, Monitor)];
                [Y|[]] -> {error, "There is such measure already."};
                _ -> {error, "Cannot add value."}
             end
      end
    end.
removeValue(Station, Date, Type, Monitor) ->
  case findStation(Station, Monitor) of
    [] -> {error, "There is no such station."};
    [X|[]] -> case getMeasure(X, Date, Type) of
               [] -> {error, "There is no such measure."};
               [Y|[]] -> [dict:store(measurements, lists:delete(Y, dict:fetch(measurements, X)), X) | lists:delete(X, Monitor)]
             end
  end.
getOneValue(Type, Date, Station, Monitor) ->
  case findStation(Station, Monitor) of
      [] -> {error, "There is no such station"};
      [X|[]] -> case getMeasure(X, Date, Type) of
               [] -> {error, "There is no such measure."};
               [Y|[]] -> dict:fetch(value, Y)
               end
  end.
getStationMean(Type, Station, Monitor) ->
  StationName = lists:last(findStation(Station, Monitor)),
  List =  [dict:fetch(value, X) || X <- lists:filter(fun(Y) -> dict:fetch(type,Y) == Type end, dict:fetch(measurements, StationName))],
  lists:sum(List)/length(List).
getDailyMean(Type, Day, Monitor) ->
  MeasurementsList = [dict:fetch(measurements, X) || X <- Monitor], % lista list measurements
  List = [ getSumOfList(X, Type, Day) || X <- MeasurementsList],
  lists:sum(List)/lists:flatlength(MeasurementsList).
getHourlyMean(Hour, Type, Station, Monitor) ->
  MeasurementsList = [dict:fetch(measurements, X) || X <- lists:last(findStation(Station, Monitor))],
  Length = length(lists:filter(fun(X) -> getHour(dict:fetch(date, X)) == Hour andalso dict:fetch(type, X) == Type end, MeasurementsList)),
  List = [dict:fetch(value, X) || X <- (lists:filter(fun(X) -> getHour(dict:fetch(date, X)) == Hour andalso dict:fetch(type, X) == Type end, MeasurementsList))],
  lists:sum(List)/Length.