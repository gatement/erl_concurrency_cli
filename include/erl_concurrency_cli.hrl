%-record(log, {id, at, resp_time}).

%% lager
-define(LOG_DEBUG(Msg, Args), lager:debug(Msg, Args)).
-define(LOG_INFO(Msg, Args), lager:info(Msg, Args)).
-define(LOG_NOTICE(Msg, Args), lager:notice(Msg, Args)).
-define(LOG_WARNING(Msg, Args), lager:warning(Msg, Args)).
-define(LOG_ERROR(Msg, Args), lager:error(Msg, Args)).
-define(LOG_CRITICAL(Msg, Args), lager:critical(Msg, Args)).
-define(LOG_ALERT(Msg, Args), lager:alert(Msg, Args)).
-define(LOG_EMERGENCY(Msg, Args), lager:emergency(Msg, Args)).
