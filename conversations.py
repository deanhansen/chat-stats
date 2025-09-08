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
                # Skip non-user system messages, but keep ones marked as user
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

    for conversation in conversations_data:
        updated = conversation.get('update_time')
        if not updated:
            continue

        updated_date = datetime.fromtimestamp(updated)

        title = conversation.get('title', 'Untitled')
        sanitized_title = re.sub(r"[^a-zA-Z0-9_]", " ", title)[:120]

        messages = get_conversation_messages(conversation)

        # Organize by month_year without creating folders
        directory_name = updated_date.strftime('%m_%Y')
        if directory_name not in pruned_data:
            pruned_data[directory_name] = []

        pruned_data[directory_name].append({
            "title": title,
            "create_time": datetime.fromtimestamp(conversation.get('create_time')).strftime('%Y-%m-%d %H:%M:%S'),
            "update_time": updated_date.strftime('%Y-%m-%d %H:%M:%S'),
            "messages": messages
        })

    with open(output_path, 'w', encoding='utf-8') as json_file:
        json.dump(pruned_data, json_file, ensure_ascii=False, indent=4)

if __name__ == "__main__":
    input_json_path = sys.argv[1]   # e.g. user uploaded JSON file path
    output_json_path = sys.argv[2]  # output parsed JSON file path

    with open(input_json_path, 'r', encoding='utf-8') as file:
        conversations_data = json.load(file)

    write_conversations_and_json(conversations_data, output_json_path)
