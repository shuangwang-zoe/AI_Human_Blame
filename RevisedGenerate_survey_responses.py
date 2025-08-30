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
                    {"role": "system", "content": "你是一个在中国内地出生并长大的成年人，正在认真参与一项关于道德判断的问卷调查。请以中文作答，回答风格应贴近真实、自然、可信，体现出你的文化背景和价值观。请避免机械化回答，尽量像真实受访者一样表达自己的想法。"},
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

    def generate_responses(self, prompt: str, num_responses: int = 200) -> List[Dict]:
        """Generate multiple survey responses"""
        responses = []
        
        for i in range(num_responses):
            print(f"Generating response {i+1}/{num_responses}")
            
            # Add some randomness to the prompt to get varied responses
            varied_prompt = f"{prompt}\n\n请从第{i+1}位参与者的角度回答。"
            
            response = self.generate_response(varied_prompt)
            if response:
                responses.append({
                    "id": i + 1,
                    "response": response
                })
            
            # Add delay to avoid rate limiting
            time.sleep(1)
        
        return responses

def parse_response(response_text: str) -> Dict:
    """Parse the response text into structured data"""
    # 初始化结果字典
    result = {
        "自主提问": ""
    }
    
    # 分割响应文本
    sections = response_text.split("---")
    
    for section in sections:
        if "自主提问" in section:
            # 解析自主提问部分
            lines = section.split("\n")
            for line in lines:
                if line.strip() and not line.startswith("###"):
                    result["自主提问"] = line.strip()
    
    return result

def main():
    # 方法1：从环境变量获取API key
    api_key = os.getenv("DEEPSEEK_API_KEY")
    if not api_key:
        print("Please set DEEPSEEK_API_KEY environment variable")
        return

    # 定义四组句子
    sentence_groups = [
        {
            "group_id": 1,
            "sentences": [
                "某人杀害陌生人",
                "某人把热咖啡倒在一个男人身上",
                "某人将一支大头针扎进一个小孩的手掌",
                "某人用棍猛打一只狗"
            ]
        },
        {
            "group_id": 2,
            "sentences": [
                "某人在那个男人面前把他送给她的礼物扔进了垃圾桶里",
                "某人路过一个秃顶的癌症患者时笑了起来",
                "某人在一个面部畸形的女人经过时盯着她看",
                "某人刻薄地评价一个超重的人的外表"
            ]
        },
        {
            "group_id": 3,
            "sentences": [
                "某人在安静的图书馆里时仍在用手机打电话聊天",
                "某人把自己的名字刻在公交车站旁的树干上",
                "某人在街上随地吐痰",
                "某人在一条人很多的街道上抽烟"
            ]
        },
        {
            "group_id": 4,
            "sentences": [
                "在春节期间，某人选择与朋友一起出行而不是拜访父母",
                "某人不给年老的父母家用",
                "某人跟妈妈买菜时没有提出要帮妈妈拿袋子",
                "某人无视爸爸的电话，并不接通"
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
        
        for group in sentence_groups:
            print(f"Processing group {group['group_id']}")
            
            # 构建每组句子的提示
            group_prompt = f"""
请对以下一组四个句子所描述的行为进行整体评估，回答以下问题：

行为列表：
{chr(10).join(f"- {sentence}" for sentence in group['sentences'])}

### 自主提问
有时你可能会觉得自己没有掌握足够的信息来判断这些行为是否值得责备。为了帮助你做出判断，假设你已经得知了以下四个问题的完整答案：
1. “这是有意为之的吗？”
2. “有没有什么理由促使某人这样做？”
3. “这个人原本能够阻止这一事件的发生吗？”
4. “这个人在日常生活中经常这样做吗？”

请思考：在了解了上述信息之后，你是否还需要进一步了解其他情况，才能做出准确的判断？

如果是，请至少提出一个你还想了解的问题，格式为：“新问题：……”。  
如果你认为以上信息已经足够做出判断，请回答：“不需要再提问，因为：……”
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
                all_responses.append(response_data)
                
                # 解析回答内容
                parsed_response = parse_response(response)
                
                # 准备Excel数据
                excel_row = {
                    "被试ID": participant_id + 1,
                    "组ID": group['group_id'],
                    "行为列表": "\n".join(group['sentences']),
                    "自主提问": parsed_response["自主提问"]
                }
                
                excel_data.append(excel_row)
            
            # 添加延迟以避免速率限制
            time.sleep(1)

    # Save JSON responses
    with open("survey_responses.json", "w", encoding="utf-8") as f:
        json.dump(all_responses, f, ensure_ascii=False, indent=2)

    # Save Excel file
    df = pd.DataFrame(excel_data)
    df.to_excel("survey_responses2.xlsx", index=False, engine='openpyxl')

    print(f"Successfully generated {len(all_responses)} responses")
    print("Saved to survey_responses2.json and survey_responses2.xlsx")

if __name__ == "__main__":
    main() 