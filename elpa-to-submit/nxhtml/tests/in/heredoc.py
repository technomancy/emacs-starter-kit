sender = 'Buffy the Vampire Slayer'
recipient = 'Spike'

print("""\
Dear %(recipient)s,

I wish you to leave Sunnydale and never return.

Not Quite Love,
%(sender)s
""" % locals())
