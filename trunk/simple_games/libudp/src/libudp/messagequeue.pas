unit messagequeue;
{$ASSERTIONS ON}

interface

const QUEUE_SIZE=1000;

type tudpdata=record
  data:array[0..1000] of byte;
  length:integer;
end;

type tmessagequeue=class
private
 qstart,qend:integer;
 qdata:array[0..QUEUE_SIZE-1] of tudpdata;

public
  function push(data:tudpdata):boolean;
  procedure pop;
  function front:tudpdata;
  function empty:boolean;
  function size:integer;
  constructor create;

end;

implementation

constructor tmessagequeue.create;
begin
 qstart:=0;
 qend:=0;
end;

function tmessagequeue.empty:boolean;
begin
 result:=qstart=qend;
end;

function tmessagequeue.size:integer;
begin
 result:=(qend-qstart+QUEUE_SIZE) mod QUEUE_SIZE;
end;

function tmessagequeue.front:tudpdata;
begin
 assert(not empty);
 result:=qdata[qstart];
end;

procedure tmessagequeue.pop;
begin
 assert(not empty);
 qstart:=(qstart+1) mod QUEUE_SIZE;
end;

function tmessagequeue.push(data:tudpdata):boolean;
begin
 if( qend-qstart+QUEUE_SIZE) mod QUEUE_SIZE=QUEUE_SIZE-2 then
   begin
     result:=false;
     exit;
   end;
  result:=true;
  qdata[qend]:=data;
  qend:=(qend+1)mod QUEUE_SIZE;
end;

end.
