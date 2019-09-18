# simple-server

## Setup

Compile using `stack build` and execute with `stack exec -- simple-server-exe`.
`simple-server` holds a list of mail in a mailbox, where each mail is a tuple of
(address, message).

## Sending a message

Send a message to an address using `POST
http://url/?addr=address&message=text`, where `address` is any string that is
accepted by a URL query value. For example, to send a message addressed to
"cat", send `POST http://url?addr=cat&message=meow`. The server will respond
with all the mail in the mailbox, including the one you just sent.

## Receiving a message

Check for new mail in an address by sending `DELETE http://url/?addr=address`
where `address` means the same as the section above. The server will respond
with all the mail in the mailbox addressed to that address. The returned mail
will then be deleted from the mailbox.
