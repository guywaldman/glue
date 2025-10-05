from generated_models import User

def main():
    mock_user = {
        "id": "123e4567-e89b-12d3-a456-426614174000",
        "registrationStatus": "active",
    }

    user = User(**mock_user)

    output = str(user)
    print(output)


if __name__ == "__main__":
    main()
