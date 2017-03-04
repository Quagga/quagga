"""
Sample production-ready settings for patchwork project.

Most of these are commented out as they will be installation dependent.

Design based on:
    http://www.revsys.com/blog/2014/nov/21/recommended-django-project-layout/
"""

from __future__ import absolute_import

import os

import django

from .base import *  # noqa

DEBUG = True
#
# Core settings
# https://docs.djangoproject.com/en/1.8/ref/settings/#core-settings
#

# Security
#
# You'll need to replace this to a random string. The following python code can
# be used to generate a secret key:
#
#      import string, random
#      chars = string.letters + string.digits + string.punctuation
#      print repr("".join([random.choice(chars) for i in range(0,50)]))

# Email
#
# Replace this with your own details

EMAIL_HOST = os.getenv('EMAIL_HOST', 'localhost')
EMAIL_PORT = os.getenv('EMAIL_PORT', 25)
EMAIL_HOST_USER = os.getenv('EMAIL_HOST_USER', '')
# password goes in pass.py
EMAIL_USE_TLS = True

DEFAULT_FROM_EMAIL = 'Patchwork <patchwork@patchwork.quagga.net>'
SERVER_EMAIL = DEFAULT_FROM_EMAIL
NOTIFICATION_FROM_EMAIL = DEFAULT_FROM_EMAIL

ADMINS = (
    ('Paul Jakma', 'paul@quagga.net'),
)

# Database
#
# If you're using a postgres database, connecting over a local unix-domain
# socket, then the following setting should work for you. Otherwise,
# see https://docs.djangoproject.com/en/1.8/ref/settings/#databases

# password goes in pass.py
DATABASES = {
    'default': {
        'ENGINE': 'django.db.backends.mysql',
        'NAME': os.environ.get('DATABASE_NAME', 'patchwork'),
        'USER': os.environ.get('DATABASE_USER', 'patchwork'),
         'HOST': '127.0.0.1',
    },
}


#
# Static files settings
# https://docs.djangoproject.com/en/1.8/ref/settings/#static-files
# https://docs.djangoproject.com/en/1.8/ref/contrib/staticfiles/#manifeststaticfilesstorage
#

STATIC_ROOT = os.environ.get('STATIC_ROOT', '/home/patchwork/htdocs/static')


if django.VERSION >= (1, 7):
    STATICFILES_STORAGE = \
        'django.contrib.staticfiles.storage.ManifestStaticFilesStorage'

ENABLE_XMLRPC = True

LANGUAGE_CODE='en-gb'
TIME_ZONE='GMT'
ALLOWED_HOSTS=['patchwork.quagga.net','testpw.quagga.net', 'http', 'http.quagga.net', '*.quagga.net']

with open("patchwork/settings/pass.py") as f:
    code = compile(f.read(), "patchwork/settings/pass.py", 'exec')
    exec(code)
