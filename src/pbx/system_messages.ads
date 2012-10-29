package System_Messages is

   type Message_Type is (Debug, Information, Error, Warning, Fatal);

   procedure Notify (Level : in Message_Type; Message : in String);
end System_Messages;
