import sys
import json
import re
from datetime import datetime

def get_conversation_messages(conversation):
    messages = []
    current_node = conversation.get("current_node")
    mapping = conversation.get("mapping", {})

    while current_node:
        node = mapping.get(current_node, {})
        message = node.get("message") if node else None
        content = message.get("content") if message else None
        author = message.get("author", {}).get("role", "") if message else ""

        # Skip tool messages
        if author == "tool":
            current_node = node.get("parent") if node else None
            continue

        if content and content.get("content_type") == "text":
            parts = content.get("parts", [])
            if parts and len(parts[0]) > 0:
                # Skip non-user system messages, unless flagged as user system message
                if author != "system" or (message.get("metadata", {}) if message else {}).get("is_user_system_message"):
                    formatted_author = (
                        "assistant" if author == "assistant" else
                        "user" if author == "system" else
                        author
                    )

                    messages.append({
                        "author": formatted_author,
                        "text": parts[0]
                    })

        current_node = node.get("parent") if node else None

    # Reverse order and add chat_seq
    messages = messages[::-1]
    for idx, msg in enumerate(messages, start=1):
        msg["chat_seq"] = idx

    return messages

def write_conversations_and_json(conversations_data, output_path):
    pruned_data = {}

    if not isinstance(conversations_data, list):
        raise ValueError("Input JSON must be a list of conversations")

    for conversation in conversations_data:
        if not isinstance(conversation, dict):
            continue  # skip junk entries

        updated = conversation.get('update_time')
        if not updated:
            continue

        try:
            updated_date = datetime.fromtimestamp(updated)
        except (OSError, ValueError, TypeError):
            continue

        title = conversation.get('title', 'Untitled')
        sanitized_title = re.sub(r"[^a-zA-Z0-9_]", " ", str(title))[:120]

        messages = get_conversation_messages(conversation)

        directory_name = updated_date.strftime('%m_%Y')
        if directory_name not in pruned_data:
            pruned_data[directory_name] = []

        pruned_data[directory_name].append({
            "title": title,
            "create_time": datetime.fromtimestamp(
                conversation.get('create_time', updated)
            ).strftime('%Y-%m-%d %H:%M:%S'),
            "update_time": updated_date.strftime('%Y-%m-%d %H:%M:%S'),
            "messages": messages
        })

    with open(output_path, 'w', encoding='utf-8') as json_file:
        json.dump(pruned_data, json_file, ensure_ascii=False, indent=4)

if __name__ == "__main__":
    try:
        input_json_path = sys.argv[1]
        output_json_path = sys.argv[2]

        with open(input_json_path, 'r', encoding='utf-8') as file:
            try:
                conversations_data = json.load(file)
            except json.JSONDecodeError as e:
                sys.stderr.write(f"Error: Invalid JSON format - {e}\n")
                sys.exit(1)

        try:
            write_conversations_and_json(conversations_data, output_json_path)
        except Exception as e:
            sys.stderr.write(f"Error processing conversations: {e}\n")
            sys.exit(1)

    except IndexError:
        sys.stderr.write("Usage: script.py <input_json_path> <output_json_path>\n")
        sys.exit(1)
