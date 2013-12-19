#!/usr/bin/php
<?php
/**
 * Basic tool for converting our database users into JSON.
 *
 **/
$db_file = "db_src/test.db";

class identity_list {
   public $identities;
}

class user {
   public $id;
   public $name = "";
   public $extension = "";
   public $groups = array();
}

class identity {
  public $identity;
  public $user;
}

function print_usage() {
  echo "Not enough parameters.";
}

$dbh = null;

try {
 $dbh = new PDO('sqlite:'.$db_file);
 $dbh->setAttribute(PDO::ATTR_ERRMODE, 
                            PDO::ERRMODE_EXCEPTION);

$identity_list = new identity_list();
$identity_list->identities = array();

$res = $dbh->query("select `identity`, `id`, `name`, `is_receptionist`, `is_service_agent`, `is_administrator`, `extension` from auth_identities left join users on auth_identities.user_id = users.id");
  while ($row = $res->fetch()) {
   $identity = new identity();
   $identity->user = new user();

   // Set fields.
   $identity->identity = $row['identity'];
   $identity->user->name = $row['name'];
   $identity->user->extension = $row['extension'];
   $identity->user->id = $row['id'];

   // Push groups
   $groups = $dbh->query("select `name` from user_groups natural join groups where uid = ".$row['id']);

  while ($group_row = $groups->fetch()) {
   array_push ($identity->user->groups, $group_row['name']);
  }

   array_push ($identity_list->identities, $identity);

  }
echo json_encode ($identity_list);

} catch (Exception $e) {
    echo 'Caught exception: ',  $e->getMessage(), "\n";
}




?>
