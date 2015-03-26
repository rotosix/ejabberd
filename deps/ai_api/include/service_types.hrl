-ifndef(_service_types_included).
-define(_service_types_included, yeah).

%% struct thriftGroup

-record(thriftGroup, {groupId :: string() | binary(),
                      name :: string() | binary(),
                      creatorUid :: string() | binary(),
                      dnd :: dict(),
                      createTime :: string() | binary()}).

-endif.
