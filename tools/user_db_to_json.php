#!/usr/bin/php
<?php
/**
 * Basic tool for converting our database users into JSON.
 *
 **/
$db_file = "db_src/test.db";

class user_list {
   public $users;
}

class user {
   public $id;
   public $name = "";
   public $peer_id = "";
   public $groups = array();
   public $identities = array();
}

class identity {
  public $identity;
}

function print_usage() {
  echo "Not enough parameters.";
}

$dbh = null;

try {
 $dbh = new PDO('sqlite:'.$db_file);
 $dbh->setAttribute(PDO::ATTR_ERRMODE,
                            PDO::ERRMODE_EXCEPTION);

$user_list = new user_list();
$user_list->users = array();

$res = $dbh->query("select distinct `id`, `name`, `extension` from auth_identities left join users on auth_identities.user_id = users.id");
  while ($row = $res->fetch()) {
   $user = new user();

   // Set fields.
   $user->name = $row['name'];
   $user->peer_id = $row['extension'];
   $user->id = $row['id'];

   // Push groups
   $groups = $dbh->query("select `name` from user_groups natural join groups where uid = ".$row['id']);

   while ($group_row = $groups->fetch()) {
    array_push ($user->groups, $group_row['name']);
   }

   // Push identities
   $identities = $dbh->query("select `identity` from auth_identities where user_id = ".$row['id']);

   while ($identity_row = $identities->fetch()) {
    array_push ($user->identities, $identity_row['identity']);
   }

   array_push ($user_list->users, $user);

  }
echo json_encode ($user_list);

} catch (Exception $e) {
    echo 'Caught exception: ',  $e->getMessage(), "\n";
}
?>
