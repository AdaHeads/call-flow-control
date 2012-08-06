package AMI.Std is
   procedure Connect (Server_Host     : in String   := "Asterisk1";
                      Server_Port     : in Positive := 5038;
                      Event_Username  : in String   := "filtertest";
                      Event_Secret    : in String   := "filtertest";
                      Action_Username : in String   := "action";
                      Action_Secret   : in String   := "reaction");
   procedure Disconnect;
end AMI.Std;
