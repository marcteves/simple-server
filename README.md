# simple-server

## Setup

Compile using `stack build` and execute with `stack exec -- simple-server-exe`.
`simple-server` holds a list of mail, where each mail is a tuple of
(addr, message).

## Sending a message

Add a message to a "mailbox" using `POST http://url/?addr=mailbox&message=text`, where
`mailbox` is any string that is accepted by a URL query value. For example, to
send a message to the mailbox addressed by "cat", send `POST
http://url?addr=cat&message=meow`. The server will respond with all the mail in
all the mailboxes.

## Receiving a message

Check for new mail in a "mailbox" by sending `DELETE http://url/?addr=mailbox`
where `mailbox` means the same as the section above. The server will respond
with all the mail in that mailbox. Mail will then be deleted from the mailbox.
