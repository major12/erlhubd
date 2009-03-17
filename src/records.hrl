% client
%   pid      clients process
%   nick
%   my_info  temporary field for NMDC only
%   role     can be regular, registered, vip, op, chief, admin or master
-record(client, { pid, nick, my_info, role = regular }).

-record(nmdc, { state = initialized,
                receiver, sender,
                lock, skey, ckey,
                version, my_info,
                nick,
                role = regular,
                supports = []}).
