# example pass file
SECRET_KEY = "aaaaaaaaaaaaaaaaaa"
EMAIL_HOST_PASSWORD = os.getenv('EMAIL_HOST_PASSWORD', '')
DATABASES['default']['PASSWORD'] = os.environ.get('DATABASE_PASSWORD', 'bbbbbbbbbbbbbbbbbbbbb');
