with Servlet.Server.Web;
with Testapi.Server;
procedure Testapi_Aws is
   Container : Servlet.Server.Web.Aws_Container;
begin
   Testapi.Server (Container);
end Testapi_Aws;
