with Servlet.Server.Web;
with TestAPI.Server;
procedure TestAPI_AWS is
   Container : Servlet.Server.Web.AWS_Container;
begin
   TestAPI.Server (Container);
end TestAPI_AWS;
