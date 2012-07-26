package AMI.Std is
   procedure Connect (Server_Host : in String   := "Asterisk1";
                      Server_Port : in Positive := 5038;
                      Username    : in String   := "test";
                      Secret      : in String   := "test");
   procedure Disconnect;
end AMI.Std;
