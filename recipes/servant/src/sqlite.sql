CREATE TRIGGER AutoGenerateGUID
AFTER INSERT ON comments
FOR EACH ROW
WHEN (NEW.uuid IS NULL)
BEGIN
   UPDATE comments SET uuid = (select hex( randomblob(4)) || '-' || hex( randomblob(2))
             || '-' || '4' || substr( hex( randomblob(2)), 2) || '-'
             || substr('AB89', 1 + (abs(random()) % 4) , 1)  ||
             substr(hex(randomblob(2)), 2) || '-' || hex(randomblob(6)) ) WHERE rowid = NEW.rowid;
END;