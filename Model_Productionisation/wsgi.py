from api import api

import settings

if __name__ == '__main__':
    api.run(
        host=settings.FLASK_IP,
        port=int(settings.IMAGE_FINGERPRINT_API_FLASK_PORT),
        debug=False
    )
