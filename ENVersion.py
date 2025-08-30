import os
import time
import json
import pandas as pd
from typing import List, Dict
from openai import OpenAI

class DeepSeekSurveyGenerator:
    def __init__(self, api_key: str, model: str = "deepseek-reasoner"):
        self.api_key = api_key
        self.model = model
        self.client = OpenAI(
            api_key=api_key,
            base_url="https://api.deepseek.com"
        )

    def generate_response(self, prompt: str) -> str:
        """Generate a single response using DeepSeek API"""
        try:
            response = self.client.chat.completions.create(
                model=self.model,
                messages=[
                    {"role": "system", "content": "You are an adult who was born and raised in the United States, and you are now thoughtfully participating in a questionnaire on moral judgment. Please respond in English. Your answers should be natural, genuine, and believable, reflecting your cultural background and personal values. Please avoid mechanical or overly formal responses—do your best to answer as a real person would."},
                    {"role": "user", "content": prompt}
                ],
                temperature=0.7,
                max_tokens=500,
                stream=False
            )
            
            return response.choices[0].message.content
        except Exception as e:
            print(f"Error generating response: {str(e)}")
            return None

def parse_response(response: str) -> Dict:
    """Parse the response to extract additional questions or confirmation"""
    if "New question:" in response:
        return {
            "自主提问": "Yes",
            "问题内容": response.split("New question:")[1].split(";")[0].strip()
        }
    else:
        return {
            "自主提问": "No",
            "问题内容": ""
        }

def main():
    # 从环境变量获取API key
    api_key = os.getenv("DEEPSEEK_API_KEY")
    if not api_key:
        print("Please set DEEPSEEK_API_KEY environment variable")
        return

    # 定义四组句子
    sentence_groups = [
        {
            "group_id": 1,
            "sentences": [
                "Someone killed a stranger",
                "Someone poured her hot coffee on a man",
                "Someone stuck a pin into a child’s palm",
                "Someone used a stick to beat a dog"
            ]
        },
        {
            "group_id": 2,
            "sentences": [
                "Someone threw a gift into the trash right in front of the man who gave her the gift",
                "Someone laughed as he passed by a cancer patient with a bald head",
                "Someone stared at a disfigured woman as she walks past",
                "Someone made cruel remarks to an overweight person about his appearance"
            ]
        },
        {
            "group_id": 3,
            "sentences": [
                "Someone had a conversation on her cell phone while walking around the quiet library",
                "Someone scratched his name into the bark of a tree near the bus stop",
                "Someone spat on the streets",
                "Someone smoked cigarettes on a crowded street"
            ]
        },
        {
            "group_id": 4,
            "sentences": [
                "Someone went on a trip with her friends instead of visiting his parents for Christmas",
                "Someone didn't contribute money to his elderly parents",
                "Someone didn't offer to help his mother carry her bags of groceries",
                "Someone ignored her father’s phone call instead of answering"
            ]
        }
    ]

    # 使用 reasoner 模型
    model = "deepseek-reasoner"
    generator = DeepSeekSurveyGenerator(api_key, model=model)
    
    all_responses = []
    excel_data = []
    
    # 为每个参与者生成所有四组的回答
    for participant_id in range(200):
        print(f"\nGenerating responses for participant {participant_id + 1}/200")
        
        # 为当前参与者生成所有组的回答
        participant_responses = []
        for group in sentence_groups:
            print(f"Processing group {group['group_id']}")
            
            # 构建每组句子的提示
            group_prompt = f"""
Please evaluate the following group of behaviors as a whole, and answer the following questions:

Behavior list:
{chr(10).join(f"- {sentence}" for sentence in group['sentences'])}

### Sometimes you might feel that you do not have enough information to judge whether certain behavior deserves blame. To help you make a judgment, assume that you have complete answers to the following four questions:

"Was this done intentionally?"

"Was there any reason that motivated the person to do this?"

"Could the person have prevented this event from happening?"

"Does this person frequently act this way in daily life?"

Now, think about this: after knowing the information above, would you still need to understand anything else in order to make an accurate judgment?

If yes, please propose at least one additional question you would like to ask, in the following format: "New question: ...; Explanation:..."
If you believe the information above is already sufficient for making a judgment, please answer: "No additional questions needed, Reason: …"
"""
            
            # 生成回答
            response = generator.generate_response(group_prompt)
            if response:
                response_data = {
                    "id": participant_id + 1,
                    "group_id": group['group_id'],
                    "response": response,
                    "sentences": group['sentences']
                }
                participant_responses.append(response_data)
                
                # 解析回答内容
                parsed_response = parse_response(response)
                
                # 准备Excel数据
                excel_row = {
                    "被试ID": participant_id + 1,
                    "组ID": group['group_id'],
                    "行为列表": "\n".join(group['sentences']),
                    "自主提问": parsed_response["自主提问"],
                    "问题内容": parsed_response["问题内容"]
                }
                
                excel_data.append(excel_row)
            
            # 添加延迟以避免速率限制
            time.sleep(1)
        
        # 将当前参与者的所有回答添加到总列表中
        all_responses.extend(participant_responses)
        
        # 每完成一个参与者就保存一次数据
        with open("ENsurvey_responses.json", "w", encoding="utf-8") as f:
            json.dump(all_responses, f, ensure_ascii=False, indent=2)
        
        df = pd.DataFrame(excel_data)
        df.to_excel("ENsurvey_responses.xlsx", index=False, engine='openpyxl')
        
        print(f"Completed participant {participant_id + 1}")

    print(f"Successfully generated {len(all_responses)} responses")
    print("Saved to ENsurvey_responses.json and ENsurvey_responses.xlsx")

if __name__ == "__main__":
    main() 