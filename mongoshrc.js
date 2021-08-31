cmdCount = 1;
host = db.serverStatus().host;
// show error trace stack
config.set("showStackTraces", true);
prompt = function () {
  line = "No:" + cmdCount++ + " ";
  uptime = "Uptime: " + db.serverStatus().uptime + " ";
  document_numbers = "Documents: " + db.stats().objects + " ";
  db_name = "Current: [ " + db.getName() + " ] ";
  return line + uptime + document_numbers + "\n" + db_name + "@" + host + "> ";
};
