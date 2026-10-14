class Käse:
    pass


def handle(error):
    try:
        raise Exception("boom") from error
    except Exception as e:
        raise e

    raise ValueError(
        "message"
    ) from None

    raise TypeError from error
