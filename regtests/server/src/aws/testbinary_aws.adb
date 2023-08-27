with Servlet.Server.Web;
with TestBinary.Server;
procedure TestBinary_AWS is
   Container : Servlet.Server.Web.AWS_Container;
begin
   TestBinary.Server (Container);
end TestBinary_AWS;
